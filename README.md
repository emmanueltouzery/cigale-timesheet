# Cigale timesheet

> "La Cigale, ayant chanté tout l'Été, se trouva fort dépourvue quand la bise fut venue."
>	-- Jean de la Fontaine, The ant and the grasshopper

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

As it's written in Haskell, it should run fine on linux and OSX; I develop it on linux and I expect installing on Windows would be [painful][] because of dependencies and in quite some places I assume unix systems.

Also on linux, installation will take a long (long, long) time, because it will build many dependencies from source (including, if needed, bootstrap a haskell->javascript compiler).

First you need to install the [stack package manager][]. You also need to install `node.js`, `npm`, the zlib, sqlite, openssl & ncurses/tinfo devel packages (see lower).

Careful! Currently `ghcjs` [is very sensitive][] to [the node version][]... 4.1.0 works. 0.10.40 works. 5.x apparently doesn't work. You can [install n] and run:

    n 4.1.0

To prevent errors like `/usr/bin/ld: cannot find -ltinfo`, run:

    sudo dnf install ncurses-devel (fedora)
    sudo apt-get install libtinfo-dev (debian/ubuntu)

On Fedora you'll also want to run:

    sudo dnf install zlib-devel openssl-devel sqlite-devel

Then to install, run:

	bash install.sh

Note that it will download and build quite some libraries so it'll run for a while.

to run:

	cigale-timesheet

(you should have `~/.local/bin` in your path)

On linux systems you'll be able to start the app graphically from the menus.

If you're running linux and install the epiphany browser (gnome-web), the app will take advantage of it and start in its own window (web application mode).

## Prefetching

Fetching data for one day can take time, if you're using network sources, like SVN and Redmine. For that reason, there is an option to prefetch the data. There is an option in the GUI, but also, if you manually run the app with `--prefetch`, it will prefetch the data for every day from the first day of the previous month to yesterday (and remove obsolete prefetch data). The application will be very fast after that. One option is to have some mechanism (for instance a cron job on unix environments) run that automatically for you on a regular basis so that the application is fast when you need it. The application will also cache data when fetching for past days.

[Main view screenshot]: https://raw.github.com/wiki/emmanueltouzery/cigale-timesheet/main.png
[Settings screenshot]: https://raw.github.com/wiki/emmanueltouzery/cigale-timesheet/settings.png
[painful]: https://plus.google.com/108801936173059193561/posts/PE3TiGMkUx2
[stack package manager]: http://docs.haskellstack.org/en/stable/README.html#how-to-install
[is very sensitive]: https://github.com/ghcjs/ghcjs/issues/451
[the node version]: https://github.com/commercialhaskell/stack/issues/1496#issuecomment-174626093
[install n]: http://askubuntu.com/questions/426750/how-can-i-update-my-nodejs-to-the-latest-version/480642#480642
