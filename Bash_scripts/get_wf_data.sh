#!/bin/bash

url_3km_data=https://haze.airfire.org/bluesky-daily/output/hysplit-pp/NAM-3km/

## Connect to ftp server
lftp url_3km_data

## get site manifest
du -a --exclude=*.png --exclude=images --exclude=deprecated --exclude=*carryover \
--exclude=*.kmz --exclude=*.tgz --exclude=*.json --exclude=*.ida --exclude=*.ems95 \
--exclude=*.csv --exclude=*.tar.gz -- exclude=2018* > NAM-3km_manifest.txt

## get_wf_data.R is used to define  file containing a list of paths to each smoke file
## Get data 
wget --continue -x -nH --cut-dirs=4 -i ./NAM-3km_paths.txt