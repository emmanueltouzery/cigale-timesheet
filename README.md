# Cigale timesheet

> "La Cigale, ayant chanté tout l'Été, se trouva fort dépourvue quand la bise fut venue."  
>	-- Jean de la Fontaine, The ant and the grasshopper

[![Build Status](https://travis-ci.org/emmanueltouzery/cigale-timesheet.png?branch=master)](https://travis-ci.org/emmanueltouzery/cigale-timesheet)

## Purpose

If you need to give a timesheet of your activities in the previous month for your work, but you didn't collect the information of what you were doing, when (like the grasshopper in the tale, who didn't plan for winter), then this program may help you.

It will look at traces of your past activity in your system. Here are the event sources that it will take into account:

* The emails you sent (mbox format, for instance Thunderbird)
* Ical sources (for instance Google calendar)
* Source control activity (GIT, HG, SVN)
* Skype chats
* Redmine bug and wiki activity

![Main view detail](https://raw.github.com/wiki/emmanueltouzery/cigale-timesheet/main-crop.png)

## Usage

The application runs as a web application on your computer.

* [Main view screenshot][]
* [Settings screenshot][]

## Installation

As it's written in Haskell, it should run fine on linux, OSX and Windows, however I develop it on linux and I expect installing on Windows would be [painful][] because of dependencies.

First you need to install the Haskell platform. Then to install, run:

	cabal install

Note that it will download and build quite some libraries so it'll run for a while.

to run:

	cigale-timesheet

(you should have ~/.cabal/bin in your path)

On linux systems you'll be able to start the app graphically from the menus.

If you're running linux and install the epiphany browser (gnome-web), the app will take advantage of it and start in its own window (web application mode).

[Main view screenshot]: https://raw.github.com/wiki/emmanueltouzery/cigale-timesheet/main.png
[Settings screenshot]: https://raw.github.com/wiki/emmanueltouzery/cigale-timesheet/settings.png
[painful]: https://plus.google.com/108801936173059193561/posts/PE3TiGMkUx2
