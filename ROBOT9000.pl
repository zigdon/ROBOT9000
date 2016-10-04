#!/usr/bin/perl -w
#
#  Enforced originality!  If someone repeats something that has been already
#  said in channel, silence them.  Silence time increasing geometrically.
#
#  Copyright (C) 2007  Dan Boger - zigdon+bot@gmail.com
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# A current copy of this code can be found at:
#
#   http://irc.peeron.com/xkcd/ROBOT9000.html
#   http://irc.peeron.com/xkcd/ROBOT9000.pl
#   http://irc.peeron.com/xkcd/ROBOT9000.yml
#   http://irc.peeron.com/xkcd/ROBOT9000.sql
#
# Changelog:
#
#   http://irc.peeron.com/xkcd/ROBOT9000.changelog.txt
#
# By default, the next mute time never goes down.  To have it decay, set up a
# cronjob such as this:
#
# 0 */6 * * * echo "update users set timeout_power = timeout_power - 1 where timeout_power > 0" | mysql -D databasename
#
#
# $Id: ROBOT9000.pl 498 2009-03-02 02:36:29Z dan $

use strict;
use Net::IRC;
use Time::HiRes qw/usleep/;
use DBI;
use Date::Calc qw/Normalize_DHMS/;
use Data::Dumper;
use YAML qw/LoadFile/;

use constant {
    DEBUG   => 0,
    VERSION => '$Id: ROBOT9000.pl 498 2009-03-02 02:36:29Z dan $'
};

# Load config file - sample file at:
# http://irc.peeron.com/xkcd/ROBOT9000.yml
my $config_file = shift or die "Usage: $0 <config> [<log file to load>]";
my $config = LoadFile($config_file);

# when is the next time someone should be unbanned?
my $next_unban = 1;

# when should we run the cleanup next?
my $maint_time = time + 20;

# when was the last time we heard *anything*
my $last_public = time;

# some annoying globals
my $topic;
my %nicks;
my %nicks_tmp;
my $nick_re;
my %nick_changes;
my %common_words;
my %sql;
my @lookup_queue;
my $paused;

# what modes are used to deop/op?
my %op_table = ( "%" => "h", "@" => "o", "~" => "q", "&" => "a" );
my %rev_op_table = reverse %op_table;

# connect to the IRC server and the database
my ( $irc, $irc_conn, $dbh ) = &setup( @ARGV ? 0 : 1 );

if (@ARGV) {    # we're only loading an existing log file, not actually running
    print "Loading log files...\n";
    &load_log;
}

&event_loop;

sub logmsg {
    print scalar localtime, " - @_\n";
    print LOG scalar localtime, " - @_\n",;
}

sub event_loop {

    #warn "event_loop (@_)\n";

    while (1) {
        $irc->do_one_loop();
        usleep 50;

        if ( $next_unban and time > $next_unban and not $paused ) {
            &process_unbans;
        }

        if ( time > $maint_time ) {
            logmsg "Running maint";
            $irc_conn->mode( $config->{irc_chan}, "+m" );

            while (@lookup_queue) {
                my @batch =
                  splice( @lookup_queue, 0, $config->{names_request_size} || 5,
                    () );
                logmsg "Looking up hostmasks... ", join ", ", @batch;
                $irc_conn->userhost(@batch);
                usleep 250;
            }

            foreach ( keys %nick_changes ) {
                next if $nick_changes{$_}[0] + 300 > time;

                logmsg "Clearing nick_changes for $_";
                delete $nick_changes{$_};
                $irc_conn->mode( $config->{irc_chan}, "-b", "~n:$_" );
            }

            $maint_time = time + 300;

            if ( time - $last_public > $config->{fortune_time} + 300 ) {
                logmsg "Seems like we're not connected. restarting";
                exit;
            } elsif ( time - $last_public > $config->{fortune_time} ) {
                $irc_conn->userhost( $config->{irc_nick} );
                logmsg "Too quiet.  Ping?";
                sleep 1;

                if ( $config->{fortune_command}
                    and -x $config->{fortune_command} )
                {
                    $irc_conn->privmsg( $config->{irc_chan}, $_ )
                      foreach ( "It's too quiet:",
                        split /\n/, `$config->{fortune_command}` );
                }
            }
        }
    }
}

sub process_unbans {
    $sql{get_unbans}->execute(time);
    while ( my ( $nick, $userhost, $id, $bantype ) =
        $sql{get_unbans}->fetchrow_array )
    {
        logmsg "Restoring $userhost";
        if ( $bantype eq 'v' ) {
            $irc_conn->mode( $config->{irc_chan}, "+v", $nick );
            $nicks{$nick} = "+" unless $nicks{$nick};
        } else {
            $irc_conn->mode( $config->{irc_chan}, "+v$op_table{$bantype}",
                $nick, $nick );
            $nicks{$nick} = $bantype;
        }
        $sql{clear_ban}->execute($id);

      #$irc_conn->privmsg( $nick, "you may now speak in $config->{irc_chan}." );
    }

    $sql{next_unban}->execute;
    ($next_unban) = $sql{next_unban}->fetchrow_array;
    $sql{next_unban}->finish;
}

sub setup {
    my $connect = shift;

    #warn "setup (@_)\n";
    # open our log file
    open LOG, ">>", $config->{logfile}
      or die "Can't write to $config->{logfile}: $!";
    logmsg "Starting up, version", VERSION;
    sleep 5;

    # connect to the database
    logmsg "Connecting dbi://$config->{db_user}\@$config->{db_name}";
    my $dbh = DBI->connect( "DBI:mysql:database=$config->{db_name}",
        $config->{db_user}, $config->{db_pass} )
      or die "Can't connect to the database!";
    $dbh->{RaiseError} = 1;

    logmsg "Preparing SQL statements";
    $sql{lookup_line} = $dbh->prepare(
        "select id from `lines`
          where msg = ?
          limit 1"
    );
    $sql{add_line} = $dbh->prepare(
        "insert into `lines` (msg)
         values (?)"
    );
    $sql{lookup_user} = $dbh->prepare(
        "select timeout_power, banned_until from users
          where mask = ?
          limit 1"
    );
    $sql{lookup_mask} = $dbh->prepare(
        "select mask 
           from users
          where nick = ?
          order by last_talk desc
          limit 1"
    );
    $sql{update_user} = $dbh->prepare(
        "update users
            set timeout_power = timeout_power + 2,
                banned_until = ?,
                nick = ?, 
                total_bans = total_bans + 1,
                ban_type = ?
          where mask = ?
          limit 1"
    );
    $sql{update_nick} = $dbh->prepare(
        "update users
            set nick = ?
          where mask = ?
          limit 1"
    );
    $sql{add_user} = $dbh->prepare(
        "insert into users (banned_until, nick, mask, timeout_power, 
                            lines_talked, total_bans, ban_type)
         values (?, ?, ?, ?, 0, 0, ?)"
    );
    $sql{user_talk} = $dbh->prepare(
        "update users
            set lines_talked = lines_talked + 1,
                word_count   = word_count + ? + 1,
                last_talk    = null
          where mask = ?
          limit 1"
    );
    $sql{next_unban} = $dbh->prepare(
        "select min(banned_until)
           from users
          where banned_until > 0"
    );
    $sql{get_unbans} = $dbh->prepare(
        "select nick, mask, id, ban_type
           from users
          where banned_until > 0
            and banned_until <= ?"
    );
    $sql{clear_ban} = $dbh->prepare(
        "update users
            set banned_until = 0
          where id = ?"
    );
    $sql{high_score} = $dbh->prepare(
"select nick, lines_talked/word_count * lines_talked/(total_bans + 1) as score
         from users
         order by lines_talked/word_count * lines_talked/(total_bans + 1) desc, lines_talked desc
         limit 1"
    );

    return ( $irc, $irc_conn, $dbh ) unless $connect;

    # log into IRC
    logmsg "Connecting irc://$config->{irc_nick}";
    $irc = new Net::IRC;
    my $irc_conn = $irc->newconn(
        Nick     => $config->{irc_nick},
        Server   => $config->{irc_server},
        Ircname  => $config->{irc_name},
        Username => $config->{irc_username} || "ROBOT9000",
    );

    if (DEBUG) {
        open DEBUG_FH, ">>$config->{logfile}.debug"
          or die "Can't write to $config->{logfile}.debug: $!";
        $irc_conn->add_default_handler( \&dump_event );
    }

    # talk events
    $irc_conn->add_handler( public  => \&irc_on_public );
    $irc_conn->add_handler( caction => \&irc_on_public );
    $irc_conn->add_handler( notice  => \&irc_on_notice );
    $irc_conn->add_handler( msg     => \&irc_on_msg );

    # user events
    $irc_conn->add_handler( nick => \&irc_on_nick );
    $irc_conn->add_handler( join => \&irc_on_joinpart );
    $irc_conn->add_handler( part => \&irc_on_joinpart );
    $irc_conn->add_handler( quit => \&irc_on_joinpart );

    # server events
    $irc_conn->add_handler( endofmotd  => \&irc_on_connect );
    $irc_conn->add_handler( nomotd     => \&irc_on_connect );
    $irc_conn->add_handler( topic      => \&irc_on_topic );
    $irc_conn->add_handler( namreply   => \&irc_on_names );
    $irc_conn->add_handler( endofnames => \&irc_on_endnames );
    $irc_conn->add_handler( mode       => \&irc_on_mode );
    $irc_conn->add_handler( userhost   => \&irc_on_userhost );
    $irc_conn->add_handler(
        chanoprivsneeded => sub {
            logmsg "Reauthing to nickserv";
            $irc_conn->privmsg( "nickserv", "identify $config->{irc_pass}" );
            $irc_conn->mode( $config->{irc_nick}, "+B" );
        }
    );

    logmsg "Setup complete";

    logmsg "Loading common words...";
    open( WORDS, $config->{common_file} )
      or die "Can't read $config->{common_file}: $!";
    while (<WORDS>) {
        chomp;
        $common_words{ lc $_ } = 1;
    }
    close WORDS;
    logmsg "Loaded ", scalar keys %common_words, " words";

    return ( $irc, $irc_conn, $dbh );
}

# event handlers
sub irc_on_connect {

    #warn "irc_on_connect (@_)\n";
    my ( $self, $event ) = @_;

    logmsg "Authenticating";
    $self->privmsg( "nickserv", "identify $config->{irc_pass}" );
    $self->mode( $config->{irc_nick}, "+B" );
    sleep 2;

    logmsg "Connected to IRC, joining $config->{irc_chan}";
    $self->join( $config->{irc_chan} );

    $irc_conn->names( $config->{irc_chan} );
}

sub irc_on_notice {
    my ( $self, $event ) = @_;
    my ( $nick, $msg ) = ( $event->nick, $event->args );

    logmsg "Notice from $nick to " . @{ $event->to }[0] . ": $msg";
    return if ${ $event->to }[0] ne $config->{irc_chan};

    &fail( $self, $nick, $event->userhost,
        "Failed for sending notices to channel" );
}

sub irc_on_msg {

    #warn "irc_on_msg (@_)\n";
    my ( $self, $event ) = @_;
    my ( $nick, $msg ) = ( $event->nick, $event->args );
    my @args;
    ( $msg, @args ) = split ' ', $msg;

    return if $nick eq $config->{irc_nick};

    logmsg "PRIVMSG $nick($nicks{$nick}): $msg @args";
    if ( lc $msg eq 'version' ) {
        $self->privmsg( $nick, VERSION );
    } elsif ( lc $msg eq 'timeout' ) {
        my ( $timeout, $banned_until );
        if ( $args[0] ) {
            if ( $sql{lookup_mask}->execute( $args[0] ) > 0 ) {
                my ($mask) = $sql{lookup_mask}->fetchrow_array;
                $sql{lookup_mask}->finish;
                ( $timeout, $banned_until ) = &get_timeout($mask);
            }
        } else {
            ( $timeout, $banned_until ) = &get_timeout( $event->userhost );
        }

        if ($timeout) {
            $timeout = &timeout_to_text( 2**( $timeout + 2 ) );

            $self->privmsg( $nick, "Next timeout will be $timeout" );

            if ($banned_until) {
                $self->privmsg( $nick,
                    "Currently muted, can speak again in "
                      . &timeout_to_text( $banned_until - time ) );
            }
        } else {
            $self->privmsg( $nick, "No timeout found" );
        }
    } elsif (
        (
            exists $config->{auth}{ lc $nick }
            and $event->userhost =~ /$config->{auth}{ lc $nick }/
        )
        or $nicks{$nick} =~ /[~@&%]/
      )
    {
        logmsg "AUTH $nick: $msg ($nicks{$nick})";
        if ( $msg eq 'quit' ) {
            $self->privmsg( $nick, "Quitting" );
            exit;
        } elsif ( $msg eq 'msg' and exists $config->{auth}{ lc $nick } ) {
            $self->privmsg( $nick, "Ok - sending $args[0]: @args[1..$#args]" );
            $self->privmsg( $args[0], join " ", @args[ 1 .. $#args ] );
            logmsg "Sending MSG to $args[0]: @args[1..$#args]";
        } elsif ( $msg eq 'unban' ) {
            logmsg "Unbanning $args[0] by command";
            $self->mode( $config->{irc_chan}, "-b", $args[0] );
        } elsif ( $msg eq 'mode' ) {
            logmsg "Setting mode @args by command";
            $self->mode( $config->{irc_chan}, @args );
        } elsif ( $msg eq 'kick' ) {
            logmsg "Kicking $args[0] by command";
            $self->kick(
                $config->{irc_chan}, $args[0],
                $args[1]
                ? join " ",
                $args[ 1 .. $#args ]
                : "Kick"
            );
        } elsif ( $msg eq 'fail' and $args[0] =~ /([^!]+)!(\S+)/ ) {
            logmsg "Failing $1!$2 by command";
            &fail(
                $self, $1, $2,
                "Failed by a live moderator",
                "$nick failed $args[0]: @args[1..$#args]"
            );
        } elsif ( $msg eq 'nick_re' ) {
            logmsg "Current nick re: $nick_re";
            $self->privmsg( $nick, "Ok, logged" );
        } elsif ( $msg eq 'names' ) {
            logmsg "Current names: ", join ", ",
              map { "$_($nicks{$_})" } sort keys %nicks;
            $self->privmsg( $nick, "Current names: ",
                join ", ", map { "$_($nicks{$_})" } sort keys %nicks );
        } elsif ( $msg eq 'fail' ) {
            if ( $sql{lookup_mask}->execute( $args[0] ) > 0 ) {
                my ($mask) = $sql{lookup_mask}->fetchrow_array;
                $sql{lookup_mask}->finish;
                logmsg "Failing $args[0]!$mask by command";
                &fail(
                    $self, $args[0], $mask,
                    "Failed by a live moderator",
                    "$nick failed $args[0]: @args[1..$#args]"
                );
            } else {
                logmsg "Couldn't find mask for $args[0]";
            }
        } elsif ( $msg eq 'check' ) {
            logmsg "Checking for pending mutes to restore";
            $self->privmsg( $nick, "Ok, processing mutes to restore" );
            &process_unbans;
        } elsif ( $msg eq 'pause' ) {
            $self->privmsg( $nick, "Ok, not voicing on join" );
            $paused = 1;
        } elsif ( $msg eq 'resume' ) {
            $self->privmsg( $nick, "Ok, voicing on join" );
            $paused = 0;
            &process_unbans;
        } else {
            foreach (
"Commands: timeout - query if you're banned, and what your next ban will be",
                "          timeout <nick> - same, for someone else",
                "          unban   - unban given nickmask",
                "          check   - check if there are any pending unmutes",
                "          kick <nick> <msg> - kick someone",
"          names - list the currently known privs of users in channel",
"          fail <nick> <msg> - have the moderator manually silence <nick>",
                "          version - report current version",
                "          pause   - stop granting voice",
                "          resume  - resume granting voice",
              )
            {
                $self->privmsg( $nick, $_ );
            }
        }
    } else {
        foreach (
"Commands: timeout <nick> - query if you're banned, and what your next ban will be",
            "          version        - report current version",
          )
        {
            $self->privmsg( $nick, $_ );
        }
        logmsg "Ignoring PRIVMSG from $nick ", $event->userhost;
    }

}

# public msg - someone talking in chat
sub irc_on_public {

    #warn "irc_on_public (@_)\n";
    my ( $self, $event ) = @_;
    my ( $nick, $userhost ) = ( $event->nick, $event->userhost );
    my ($msg) = ( $event->args );

    $last_public = time;
    if ( $nick eq $config->{irc_nick} ) {
        logmsg "*** Still connected, it seems";
        return;
    }

    logmsg "$nick: $msg";
    my $length = length $msg;

    if ( $config->{comic_fail} and uc $msg eq $msg and $msg =~ /^\(#.{10}\) / )
    {
        &fail(
            $self, $nick, $userhost,
            "Your IRC client is not allowed here",
            "Microsoft chat comics mode banned"
        );
        $self->kick( $config->{irc_chan}, $nick, "Come back later" );
        return;
    }

    # process the message so that we strip them down
    $msg = &strip($msg);

    if (   $length == 0
        or $length > 10 and length($msg) / $length < $config->{signal_ratio} )
    {
        &fail(
            $self, $nick, $userhost,
            "Not enough content",
            "Not enough content: " . length($msg) . " vs $length"
        );
        return;
    }

    # check if the line was already in the DB
    my $res = $sql{lookup_line}->execute($msg);

    if ( $res > 0 ) {

        # kick!
        &fail( $self, $nick, $userhost );
    } else {

        # add it as a new line
        $sql{add_line}->execute($msg);

        my $words = ( $msg =~ tr/ / / );
        if ( $sql{user_talk}->execute( $words, $userhost ) == 0 ) {
            $sql{add_user}->execute( 0, $nick, $userhost, 0, "v" );
            $sql{user_talk}->execute( $words, $userhost );
        }
    }

    $sql{lookup_line}->finish;
}

sub strip {
    my $msg = shift;

    # remove case
    $msg = lc $msg;

    # remove addressing nicks:
    $msg =~ s/^\S+: ?//;

    # remove any nicks referred to
    $msg =~ s/(?:^|\W\K)(?:$nick_re)(?=\W|$)/ /g if $nick_re;

    # remove control chars
    $msg =~ s/[[:cntrl:]]//g;

    # remove smilies
    $msg =~
s/(?:^|\s)(?:[[:punct:]]+\w|[[:punct:]]+\w|[[:punct:]]+\w[[:punct:]]+)(?:\s|$)/ /g;

    # remove punct
    $msg =~ s/([a-zA-Z])'([a-zA-Z])/$1$2/g;
    $msg =~ s/[^a-zA-Z\d -]+/ /g;

    # remove lone '-'
    $msg =~ s/(?<!\w)-+|-+(?!\w)/ /g;

    # repeating chars
    $msg =~ s/(.)\1{2,}/$1$1/g;
    $msg =~ s/(..)\1{2,}/$1$1/g;

    # removing leading/trailing/multiple spaces
    $msg =~ s/^\s+|\s+$//g;
    $msg =~ s/\s\s+/ /g;

    return $msg;
}

sub get_timeout {
    my $mask = shift;

    $sql{lookup_user}->execute($mask);
    my ( $timeout, $banned_until ) = $sql{lookup_user}->fetchrow_array;
    $sql{lookup_user}->finish;

    return ( $timeout, $banned_until );
}

sub timeout_to_text {
    my $timeout = shift;

    my ( $dd, $dh, $dm, $ds ) = Normalize_DHMS( 0, 0, 0, $timeout );
    my $delta_text;
    $delta_text .= "$dd day" .    ( $dd == 1 ? " " : "s " ) if $dd;
    $delta_text .= "$dh hour" .   ( $dh == 1 ? " " : "s " ) if $dh;
    $delta_text .= "$dm minute" . ( $dm == 1 ? " " : "s " ) if $dm;
    $delta_text .= "$ds second" . ( $ds == 1 ? " " : "s " ) if $ds;
    $delta_text =~ s/ $//;

    return $delta_text;
}

# fail - silence for 2**2n
sub fail {

    my ( $self, $nick, $userhost, $msg, $opmsg ) = @_;

    logmsg "Failing $nick ($userhost)";
    logmsg "msg: $msg"     if $msg;
    logmsg "opmsg: $opmsg" if $opmsg;

    # look up the last timeout value for this userhost, default is 1
    my ( $timeout, $banned_until ) = &get_timeout($userhost);

    $timeout += 2;

    # someone abusing the system in some way
    if ( 2**$timeout > $config->{timeout_limit} ) {
        logmsg "Kickbanning $nick ($userhost)";
        $self->notice( $config->{irc_chan}, "$nick, thanks for playing!" );
        $self->mode( $config->{irc_chan}, "+b", $userhost );
        $self->kick( $config->{irc_chan}, $nick, "Go away" );
        return;
    }

    my $delta_text = &timeout_to_text( 2**$timeout );

    if ($msg) {
        $self->notice( $config->{irc_chan},
            "$nick, you have been muted for $delta_text: $msg" );
        $self->notice( "\@$config->{irc_chan}", $opmsg ) if $opmsg;
    } elsif ( not $banned_until or $banned_until <= 1 ) {
        $self->notice( $config->{irc_chan},
            "$nick, you have been muted for $delta_text." );
        $self->notice( "\@$config->{irc_chan}", $opmsg ) if $opmsg;
    }

    my $bantype = "v";
    if ( not $nicks{$nick} or $nicks{$nick} eq '+' or $nicks{$nick} eq '1' ) {
        $self->mode( $config->{irc_chan}, "-v", $nick );
    } else {
        if ( exists $op_table{ $nicks{$nick} } ) {
            logmsg
"$nick is an operator ($nicks{$nick}) - deopping first (-$op_table{$nicks{$nick}})";
            $self->mode( $config->{irc_chan}, "-v$op_table{$nicks{$nick}}",
                $nick, $nick );
        } else {
            logmsg "$nick is an operator ($nicks{$nick}) - can't deop";
            $self->mode( $config->{irc_chan}, "-v", $nick );
        }
        $bantype = $nicks{$nick};
    }

    my $target = time + 2**$timeout;

    if (
        $sql{update_user}->execute( $target, $nick, $bantype, $userhost ) == 0 )
    {
        $sql{add_user}->execute( $target, $nick, $userhost, 2, $bantype );
    }
    logmsg "Silenced for " . ( 2**$timeout ) . " seconds";

    if ( not $next_unban or $target < $next_unban ) {
        $next_unban = $target;
    }

    # if someone gets failed while already muted, just punt them
    if (    $banned_until
        and $banned_until > 1
        and ( not defined $msg or $msg ne 'Failed by a live moderator' ) )
    {
        $self->kick( $config->{irc_chan}, $nick, "Come back later" );
        logmsg "Kicking $nick for getting muted while muted";
    }
}

sub kick {

    #warn "kick (@_)\n";
    my ( $self, $nick, $userhost, $msg ) = @_;

    &fail( $self, $nick, $userhost, $msg );

    $msg ||= "Go away";

    logmsg "Kicking $nick ($userhost): $msg";

    $self->kick( $config->{irc_chan}, $nick, $msg );
}

sub load_log {
    while (<>) {

        # http://isomerica.net/~xkcd/#xkcd.log
        # 20:50 <@zigdon> oh, right, he can't actually kick you
        # 20:56  * zigdon tests
        #

        next unless s/.*?[>*] //;
        chomp;

        my $msg = &strip($_);

        my $res = $sql{lookup_line}->execute($msg);
        next if $res > 0;
        print "$msg\n";
        $sql{add_line}->execute($msg);
    }
    exit;
}

sub update_nick_re {
    $nick_re = $config->{irc_nick};
    $nick_re .= "|\Q$_\E"
      foreach grep { not exists $common_words{$_} } keys %nicks;
    $nick_re = qr/$nick_re/i;

    #logmsg "Nick_re = $nick_re";
}

sub irc_on_nick {
    my ( $self, $event ) = @_;
    my ( $oldnick, $newnick, $userhost ) =
      ( lc $event->nick, $event->args, $event->userhost );
    $newnick = lc $newnick;

    $last_public = time;

    $nicks{$newnick} = $nicks{$oldnick};
    delete $nicks{$oldnick};

    # make sure no one tries to nick to one of the ignored common words
    if ( exists $common_words{ lc $newnick } ) {
        $self->mode( $config->{irc_chan}, "+b", "$newnick!*@*" );
        $self->kick( $config->{irc_chan}, $newnick,
            "Please select a different nick." );
        return;
    }

    # if they're banned, we need to update the table with their new nick
    if ( $sql{update_nick}->execute( $newnick, $userhost ) > 0 ) {
        logmsg "Nick updated in database";
    }

# if someone changes nicks too often (more than 3 times in a maint period), that's a fail
    if ( exists $nick_changes{$userhost} ) {
        $nick_changes{$userhost}[0] = time;

        if ( $nick_changes{$userhost}[1]++ > 1 ) {
            my ( $timeout, $banned_until );
            if ( ( $timeout, $banned_until ) = &get_timeout($userhost)
                and $banned_until )
            {
                $self->mode( $config->{irc_chan}, "+b", "~n:$userhost" );
                &fail( $self, $newnick, $userhost,
                    "Failed for changing nicks too often" );
            }
        } elsif ( $nick_changes{$userhost}[1] > 5 ) {
            &kick( $self, $newnick );
        }
    } else {
        $nick_changes{$userhost} = [ time, 1 ];
    }

    logmsg
      "$oldnick is now known as $newnick ($nick_changes{$userhost}[1] since ",
      scalar localtime $nick_changes{$userhost}[0], ")";
    &update_nick_re;
}

sub irc_on_joinpart {
    my ( $self, $event ) = @_;
    my ($nick) = lc $event->nick;

    $last_public = time;

    my $action;
    if ( $event->{type} eq 'join' ) {

        # make sure no one tries to join as one of the ignored common words
        if ( exists $common_words{ lc $nick } ) {
            $self->kick( $config->{irc_chan}, $nick,
                "Please select a different nick." );
            return;
        }

        $nicks{$nick} = 1;
        $action = "joined";

        # make sure the DB has the correct nick for this user
        $sql{update_nick}->execute( $nick, $event->userhost );

        # if this is a new user, give them voice after a minute (disabled)
        # if it's an existing user, and they're not currently banned, give them
        # voice immediately
        if ( $sql{lookup_user}->execute( $event->userhost ) > 0
            or not $config->{welcome_msg} )
        {
            my ( $power, $ban ) = $sql{lookup_user}->fetchrow_array;
            $sql{lookup_user}->finish;
            if ( not $ban and not $paused ) {
                $irc_conn->mode( $config->{irc_chan}, "+v", $nick );
                $nicks{$nick} = "+" unless $nicks{$nick};
            }
        } else {
            $sql{add_user}->execute( time + $config->{welcome_time},
                $nick, $event->userhost, 0, "v" );
            if ( not $next_unban
                or time + $config->{welcome_time} < $next_unban )
            {
                $next_unban = time + $config->{welcome_time};
            }
            $irc_conn->privmsg( $nick, $config->{welcome_msg} );
        }
    } else {
        delete $nicks{$nick};
        $action = "left";
    }
    logmsg "$nick has $action the channel";
    &update_nick_re;
}

sub irc_on_names {
    my ( $self, $event ) = @_;
    my ( $nick, $mynick ) = ( $event->nick, $self->nick );
    my ($names) = ( $event->args )[3];

    print "Event: $_[1]->{type}\n";
    print DEBUG_FH Dumper [ @_[ 1 .. $#_ ] ] if DEBUG;

    %nicks_tmp =
      ( %nicks_tmp, map { s/^(\W)//; ( $_ => $1 ? $1 : 1 ) } split ' ',
        $names );
    logmsg "Got more names - current total: ", scalar keys %nicks_tmp;
}

sub irc_on_endnames {
    my ( $self, $event ) = @_;

    print "Event: $_[1]->{type}\n";
    print DEBUG_FH Dumper [ @_[ 1 .. $#_ ] ] if DEBUG;

    if ( keys %nicks_tmp ) {
        %nicks     = (%nicks_tmp);
        %nicks_tmp = ();
        &update_nick_re;

        # look up everyone without a voice, see if they should be +v'ed
        foreach my $nick ( keys %nicks ) {
            next if $nicks{$nick} ne '1' or $nick eq $config->{irc_nick};
            push @lookup_queue, $nick;
        }
    }

    logmsg "Names done - in channel: ", join ", ",
      map { "$_($nicks{$_})" } sort keys %nicks;
}

# we asked the userhost of a nick - this means we want to know if they should
# be +v'ed.
sub irc_on_userhost {
    my ( $self, $event ) = @_;
    my @users = split ' ', ( $event->args )[1];

    $last_public = time;
    logmsg "userhost reply for: ", join ", ", @users;

    foreach my $user (@users) {
        my ( $nick, $mask ) = split /=\+/, $user;
        logmsg "looking up $nick for possible +v";

        $sql{lookup_user}->execute($mask);
        my ( $timeout, $banned_until ) = $sql{lookup_user}->fetchrow_array;
        $sql{lookup_user}->finish;

        next if $banned_until and $banned_until > time;
        $self->mode( $config->{irc_chan}, "+v", $nick );
        $nicks{$nick} = "+" unless $nicks{$nick};
        logmsg "restoring ${nick}'s +v";
    }
}

sub irc_on_topic {
    my ( $self, $event ) = @_;

    $topic = ( $event->args )[2];
    logmsg "Topic updated to '$topic'";
}

sub irc_on_mode {
    my ( $self, $event ) = @_;

    $last_public = time;
    logmsg "Mode from", $event->nick, ":", $event->args;

    return if $event->nick eq $config->{irc_nick};

    my ( $mode, @nicks ) = ( $event->args );

    if ( $mode eq '+R' ) {
        $paused = 1;
        logmsg "join flood detected: not voicing on join.";
    }
    if ( $mode eq '-R' ) {
        $paused = 0;
        logmsg "join flood released: voicing on join.";
    }

    while (
        (
            $event->nick eq 'ChanServ'
            or ( $nicks{ $event->nick } and $nicks{ $event->nick } =~ /[@&~]/ )
        )
        and $mode =~ s/([-+])([hoqa])/$1/
        and my $nick = shift @nicks
      )
    {
        if ( $1 eq '+' ) {
            if ( exists $rev_op_table{$2} ) {
                logmsg "Marking $nick as an op ($2 - $rev_op_table{$2})";
                $nicks{$nick} = $rev_op_table{$2};
            } else {
                logmsg "Marking $nick as an op ($2 - unknown ~)";
                $nicks{$nick} = "~";
            }
        } else {
            logmsg "Unmarking $nick as an op ($2 - $rev_op_table{$2})";
            $nicks{$nick} = "+";
        }
    }
}

sub dump_event {
    logmsg "Event: $_[1]->{type} from ", $_[1]->nick, " (",
      join( ", ", $_[1]->args ), ")\n";
    print DEBUG_FH Dumper [ @_[ 1 .. $#_ ] ] if DEBUG;
}

