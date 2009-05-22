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
