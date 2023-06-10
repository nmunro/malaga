# malaga-drift-trader 0.0.1

## Instructions

### Requirements

 - Linux server
 - MySQL
 - sbcl
 - git
 - curl
 
### Needed environmental variables

 - MALAGA_DROPBOX_LOCATION
 - MALAGA_CARD_DATA
 - MALAGA_DB
 - MALAGA_LOCK
 - MALAGA_BULK_DATA
 - MALAGA_SETS
 - MALAGA_MYSQL_USERNAME
 - MALAGA_MYSQL_PASSWORD
 - MALAGA_PORT
 
The value for `MALAGA_BULK_DATA` is "https://api.scryfall.com/bulk-data"
The value for `MALAGA_SETS` is "https://api.scryfall.com/sets"

### Install Quicklisp

Quicklisp must be installed outside of a user home folder, and `/opt` is as good a place as any.

    # curl https://beta.quicklisp.org/quicklisp.lisp -o /opt/quicklisp.lisp
    # sbcl --load /opt/quicklisp.lisp --eval '(quicklisp-quickstart:install :path "/opt/quicklisp/")' --quit
    # sbcl --load /opt/quicklisp.lisp --eval '(ql:add-to-init-file)' --quit
    
### Installing malaga-drift-trader

    # cd /opt/quicklisp/local-projects
    # git clone https://github.com/nmunro/malaga-drift-trader
    
### Set up the environmental variables
    
### Building the binaries

While the main web application will be run as an interpreted lisp project, a number of tools (migration, syncing scryfall, syncing player data) can be compiled as a stand alone application for speed reason. They will be built and then moved to another location (/opt/malaga/bin) as build artifacts should be kept outside the scripts folder.

First make the `/opt/malaga/bin` directory

    # mkdir -p /opt/malaga/bin
    # cd /opt/quicklisp/local-projects/malaga-drift-trader/src/scripts
    # ./build-migrate.sh
    # mv migrate /opt/malaga/bin/
    # ./build-sync-player-data.sh
    # mv sync-scryfall-data /opt/malaga/bin/
    # ./build-sync-scryfall-data.sh
    # mv sync-player-data /opt/malaga/bin/

### Installing Systemd services

Copy `/opt/quicklisp/local-projects/malaga-drift-trader/src/systemd/malaga.service` to `/etc/systemd/system/malaga.service` (or wherever it needs to be if it differs).

Run `sudo systemctl edit malaga` to edit the file `/etc/systemd/system/malaga.service.d/override.conf` and populate it with the following:

```
[Service]
Environment="MALAGA_DROPBOX_LOCATION="
Environment="MALAGA_CARD_DATA="
Environment="MALAGA_DB="
Environment="MALAGA_LOCK="
Environment="MALAGA_BULK_DATA=https://api.scryfall.com/bulk-data"
Environment="MALAGA_SETS=https://api.scryfall.com/sets"
Environment="MALAGA_MYSQL_USERNAME="
Environment="MALAGA_MYSQL_PASSWORD="
Environment="MALAGA_PORT=5000"
```

You will need to provide values for the environmental variables.

## Author

nmunro

## Licence

BSD3-Clause
