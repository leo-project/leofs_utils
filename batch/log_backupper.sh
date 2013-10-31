#!/usr/bin/env bash

if [ "$1" == "--help" ]; then
  echo "Log Backupper ver.0.1"
  echo "  Usage: log_backupper.sh [DATE] [DAYS]"
  echo "     DATE: Log of target date. Default is yesterday. (ex:20130930)"
  echo "     DAYS: Number of days to retain archive files. Default is DURATION value. (ex:10)"
fi

## Configuration
LOG_DIR= # set log directory(required)
SEND_DIR= # set backup directory(required)
DURATION=30 # set default expire days
DUR_YMD=$(date --date $DURATION' days ago' '+%Y%m%d')
LOG_HMS=$(date '+%H%M%S')

if [ -n "$1" ]; then
  YMD=$1
else
  YMD=$(date --date 'yesterday' '+%Y%m%d')
fi

if [ -n "$2" ]; then
  DUR_YMD=$(date --date $2' days ago' '+%Y%m%d')
else
  DUR_YMD=$(date --date $DURATION' days ago' '+%Y%m%d')
fi

cd $LOG_DIR

# info
if ls info.$YMD.* > /dev/null 2>&1; then
  tar czvf $SEND_DIR/info.$YMD-$LOG_HMS.tar.gz info.$YMD.*
  if [ -e $SEND_DIR/info.$YMD-$LOG_HMS.tar.gz ]; then
    rm -f $LOG_DIR/info.$YMD.*
  fi
fi

# error
if ls error.$YMD.* > /dev/null 2>&1; then
  tar czvf $SEND_DIR/error.$YMD-$LOG_HMS.tar.gz error.$YMD.*
  if [ -e $SEND_DIR/error.$YMD-$LOG_HMS.tar.gz ]; then
    rm -f $LOG_DIR/error.$YMD.*
  fi
fi

# access
if ls access.$YMD.* > /dev/null 2>&1; then
  tar czvf $SEND_DIR/access.$YMD-$LOG_HMS.tar.gz access.$YMD.*
  if [ -e $SEND_DIR/access.$YMD-$LOG_HMS.tar.gz ]; then
    rm -f $LOG_DIR/access.$YMD.*
  fi
fi

# delete archive files
ls $SEND_DIR | while read list; do
  if [[ $list =~ [0-9]{8} ]]; then
    E_YMD=`expr $list : ".*\([0-9]\{8\}\)-.*.tar.gz"`
    if [ -n "$E_YMD" ] && [ $E_YMD -lt $DUR_YMD ]; then
      rm -f $SEND_DIR/$list
    fi
  fi
done

