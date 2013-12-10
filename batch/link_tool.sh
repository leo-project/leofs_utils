#!/usr/bin/env bash

if [ "$1" != "-i" ] && [ "$1" != "-c" ] && [ "$1" != "-b" ] || [ ! -n "$4" ]; then
  echo "Data File Linker"
  echo "  Usage: link_tool.sh [-i|-c|-b] LEOFS_DIR SHARED_DIR BACKUP_DIR"
  echo "    -i        : At first use this option. Copy etc, log and work directory"
  echo "                                                    from original directory to shared directory"
  echo "    -c        : Change symbolic link"
  echo "    -b        : Backup SHARED_DIR files"
  echo "    LEOFS_DIR : Target LeoFS home directory (ex: /usr/local/leofs/0.16.5)"
  echo "    SHARED_DIR: Common directory of configuration and data files (ex: /usr/local/leofs/shared)"
  echo "    BACKUP_DIR: Backup directory of old configuration and data files (ex: /usr/local/leofs/backup)"
  exit 1
fi

export LANG=C
LEO_DIR=$2
SHARED_DIR=$3
BACKUP_DIR=$4
DATE=$(date '+%Y%m%d_%H%M%S')

# Check LeoFS home directory exists
if [ ! -d $LEO_DIR ]; then
  echo "ERROR: LeoFS home directory not found"
  exit 1
fi

# Check send directory exists
if [ ! -d $SHARED_DIR ]; then
  mkdir -p $SHARED_DIR
  echo "[INFO] Make shared directory: $SHARED_DIR"
fi

# Check backup directory exists
if [ ! -d $BACKUP_DIR ]; then
  mkdir -p $BACKUP_DIR
  echo "[INFO] Make backup directory: $BACKUP_DIR"
fi

if [ "$1" == "-i" ]; then
  echo "[INFO] Initial preparate..."
  # Initial copy
  if [ ! -d $SHARED_DIR/leo_gateway ]; then
    mkdir -p $SHARED_DIR/leo_gateway
    cp -r $LEO_DIR/leo_gateway/{etc,log,work} $SHARED_DIR/leo_gateway
    echo "[INFO] Copy gateway files to shared directory"
  fi
  if [ ! -d $SHARED_DIR/leo_manager_0 ]; then
    mkdir -p $SHARED_DIR/leo_manager_0
    cp -r $LEO_DIR/leo_manager_0/{etc,log,work} $SHARED_DIR/leo_manager_0
    echo "[INFO] Copy manager_0 files to shared directory"
  fi
  if [ ! -d $SHARED_DIR/leo_manager_1 ]; then
    mkdir -p $SHARED_DIR/leo_manager_1
    cp -r $LEO_DIR/leo_manager_1/{etc,log,work} $SHARED_DIR/leo_manager_1
    echo "[INFO] Copy manager_1 files to shared directory"
  fi
  if [ ! -d $SHARED_DIR/leo_storage ]; then
    mkdir -p $SHARED_DIR/leo_storage
    cp -r $LEO_DIR/leo_storage/{etc,log,work} $SHARED_DIR/leo_storage
    echo "[INFO] Copy storage files to shared directory"
  fi

  # Backup original data
  if [ ! -d $LEO_DIR/leo_gateway/org ]; then
    mkdir -p $LEO_DIR/leo_gateway/org
    mv $LEO_DIR/leo_gateway/{etc,log,work} $LEO_DIR/leo_gateway/org
    echo "[INFO] Backup gateway files to backup directory"
  fi
  if [ ! -d $LEO_DIR/leo_manager_0/org ]; then
    mkdir -p $LEO_DIR/leo_manager_0/org
    mv $LEO_DIR/leo_manager_0/{etc,log,work} $LEO_DIR/leo_manager_0/org
    echo "[INFO] Backup manager_0 files to backup directory"
  fi
  if [ ! -d $LEO_DIR/leo_manager_1/org ]; then
    mkdir -p $LEO_DIR/leo_manager_1/org
    mv $LEO_DIR/leo_manager_1/{etc,log,work} $LEO_DIR/leo_manager_1/org
    echo "[INFO] Backup manager_1 files to backup directory"
  fi
  if [ ! -d $LEO_DIR/leo_storage/org ]; then
    mkdir -p $LEO_DIR/leo_storage/org
    mv $LEO_DIR/leo_storage/{etc,log,work} $LEO_DIR/leo_storage/org
    echo "[INFO] Backup storage files to backup directory"
  fi

  # Link
  if [ ! -L $LEO_DIR/leo_gateway/etc ]; then
    ln -s $SHARED_DIR/leo_gateway/etc $LEO_DIR/leo_gateway
    echo "[INFO] Make etc directory link for gateway"
  fi
  if [ ! -L $LEO_DIR/leo_manager_0/etc ]; then
    ln -s $SHARED_DIR/leo_manager_0/etc $LEO_DIR/leo_manager_0
    echo "[INFO] Make etc directory link for manager_0"
  fi
  if [ ! -L $LEO_DIR/leo_manager_1/etc ]; then
    ln -s $SHARED_DIR/leo_manager_1/etc $LEO_DIR/leo_manager_1
    echo "[INFO] Make etc directory link for manager_1"
  fi
  if [ ! -L $LEO_DIR/$LEO_VER/leo_storage/etc ]; then
    ln -s $SHARED_DIR/leo_storage/etc $LEO_DIR/leo_storage
    echo "[INFO] Make etc directory link for storage"
  fi
fi

if [ "$1" == "-c" ]; then
  echo "[INFO] Change links of directory..."
  # Change Link
  if [ -d $SHARED_DIR/leo_gateway ]; then
    rm -f $SHARED_DIR/leo_gateway/log/app/{access,error,info}
    echo "[INFO] Delete links(access, error, info) of log for gateway"
    find $SHARED_DIR/leo_gateway/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for gateway"
    ls -l $SHARED_DIR/leo_storage/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $SHARED_DIR/leo_storage/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for gateway"
  fi

  if [ -d $SHARED_DIR/leo_manager_0 ]; then
    rm -f  $SHARED_DIR/leo_manager_0/log/app/{error,info}
    echo "[INFO] Delete links(error, info) of log for manager_0"
    find $SHARED_DIR/leo_manager_0/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for manager_0"
    ls -l $SHARED_DIR/leo_manager_0/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $SHARED_DIR/leo_manager_0/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for manager_0"
  fi

  if [ -d $SHARED_DIR/leo_manager_1 ]; then
    rm -f  $SHARED_DIR/leo_manager_1/log/app/{error,info}
    echo "[INFO] Delete links(error, info) of log for manager_1"
    find $SHARED_DIR/leo_manager_1/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for manager_1"
    ls -l $SHARED_DIR/leo_manager_1/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $SHARED_DIR/leo_manager_1/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for manager_1"
  fi

  if [ -d $SHARED_DIR/leo_storage ]; then
    rm -f  $SHARED_DIR/leo_storage/log/app/{error,info}
    echo "[INFO] Delete links(error, info) of log for storage"
    find $SHARED_DIR/leo_storage/work/queue -type l | while read DIR
    do
      rm -f $DIR
    done
    echo "[INFO] Delete links of queue for storage"
    ls -l $SHARED_DIR/leo_storage/work/queue | grep 'drwx' | awk '{print $9}' | while read DIR
    do
      find $SHARED_DIR/leo_storage/work/queue/$DIR/* -type d | while read LINK_DIR
      do
        echo $LINK_DIR | awk -F '/' '{a=$(NF); sub(/_.*/, "", a); for (i=1; i<NF; i++) {b=b$i"/"}; print "ln -s "$0" " b a}' | sh
      done
    done
    echo "[INFO] Make links of queue for storage"
  fi
fi

if [ "$1" == "-b" ]; then
  # Backup old data
  mkdir -p $BACKUP_DIR/$DATE
  cp -r $SHARED_DIR/leo_* $BACKUP_DIR/$DATE
fi
exit 0
