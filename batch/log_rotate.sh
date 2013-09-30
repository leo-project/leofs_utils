#!/usr/bin/env bash

## Configuration
LOG_DIR= # set log directory
SEND_DIR= # set backup directory

if [ -n "$1" ]; then
  YMD=$1
else
  YMD=$(date --date 'yesterday' '+%Y%m%d')
fi
echo $YMD
cd $LOG_DIR

# info
if ls info.$YMD.* > /dev/null 2>&1; then
  tar czvf $SEND_DIR/info.$YMD.tar.gz info.$YMD.*
  rm -f $LOG_DIR/info.$YMD.*
fi

# error
if ls error.$YMD.* > /dev/null 2>&1; then
  tar czvf $SEND_DIR/error.$YMD.tar.gz error.$YMD.*
  rm -f $LOG_DIR/error.$YMD.*
fi

# access
if ls access.$YMD.* > /dev/null 2>&1; then
  tar czvf $SEND_DIR/access.$YMD.tar.gz access.$YMD.*
  rm -f $LOG_DIR/access.$YMD.*
fi
