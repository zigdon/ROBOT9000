-- phpMyAdmin SQL Dump
-- version 2.11.6
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: May 22, 2009 at 06:08 PM
-- Server version: 5.0.51
-- PHP Version: 5.2.6-3

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

--
-- Database: `xkcd`
--

-- --------------------------------------------------------

--
-- Table structure for table `lines`
--

CREATE TABLE IF NOT EXISTS `lines` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `msg` text NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `msg` (`msg`(32))
) ENGINE=MyISAM  DEFAULT CHARSET=latin1;

--
-- Table structure for table `users`
--

CREATE TABLE IF NOT EXISTS `users` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `mask` varchar(255) default NULL,
  `nick` varchar(64) NOT NULL default '',
  `timeout_power` int(10) unsigned NOT NULL default '0',
  `banned_until` int(10) unsigned NOT NULL default '0',
  `ban_type` char(1) NOT NULL default 'v',
  `lines_talked` int(10) unsigned NOT NULL default '0',
  `total_bans` int(10) unsigned NOT NULL default '0',
  `word_count` int(10) unsigned NOT NULL default '0',
  `last_talk` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `user_status` tinyint(3) unsigned NOT NULL default '0',
  PRIMARY KEY  (`id`),
  KEY `mask` (`mask`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1;
