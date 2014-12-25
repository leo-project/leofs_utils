#!/usr/bin/env bash

export LANG=C

if [ ! -n "$1" ]; then
  echo "Link Changer"
  echo "  Usage: link_changer.sh [ LEOFS_DIR | AVS_DIR ]"
  echo "    LEOFS_DIR : Target LeoFS home directory (ex: /usr/local/leofs/1.0.0)"
  echo "    AVS_DIR   : LeoFS avs file directory (ex: /usr/local/leofs/1.0.0/leo_storage/avs)"
  exit 1
fi

LEO_DIR=$1

# Check LeoFS home directory exists
if [ -d $LEO_DIR/leo_gateway ]; then
  echo "[INFO] Change links of directory..."
  # Change Link
  if [ -d $LEO_DIR/leo_gateway ]; then
    rm -f $LEO_DIR/leo_gateway/log/app/{access,error,info}
    echo "[INFO] Delete links(access, error, info) of log for gateway"
    find $LEO_DIR/leo_gateway/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for gateway"
    ls -l $LEO_DIR/leo_gateway/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $LEO_DIR/leo_gateway/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for gateway"
  fi

  if [ -d $LEO_DIR/leo_manager_0 ]; then
    rm -f  $LEO_DIR/leo_manager_0/log/app/{error,info}
    echo "[INFO] Delete links(error, info) of log for manager_0"
    find $LEO_DIR/leo_manager_0/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for manager_0"
    ls -l $LEO_DIR/leo_manager_0/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $LEO_DIR/leo_manager_0/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for manager_0"
  fi

  if [ -d $LEO_DIR/leo_manager_1 ]; then
    rm -f  $LEO_DIR/leo_manager_1/log/app/{error,info}
    echo "[INFO] Delete links(error, info) of log for manager_1"
    find $LEO_DIR/leo_manager_1/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for manager_1"
    ls -l $LEO_DIR/leo_manager_1/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $LEO_DIR/leo_manager_1/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for manager_1"
  fi

  if [ -d $LEO_DIR/leo_storage ]; then
    rm -f  $LEO_DIR/leo_storage/log/app/{error,info}
    echo "[INFO] Delete links(error, info) of log for storage"
    find $LEO_DIR/leo_storage/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for storage"
    ls -l $LEO_DIR/leo_storage/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $LEO_DIR/leo_storage/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for storage"
  fi
elif [ -d $LEO_DIR/metadata ]; then
  find $LEO_DIR -type l | while read DIR
  do
    rm -f $DIR
  done
  echo "[INFO] Delete links of metadata and object for storage"
  find $LEO_DIR/metadata/* -type d | while read LINK_DIR
  do
    echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
  done
  find $LEO_DIR/object/* -type f | while read LINK_DIR
  do
    echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
  done
  echo "[INFO] Make links of metadata and object for storage"
else
  echo "[ERROR] Not doing anything"
fi
exit 0
