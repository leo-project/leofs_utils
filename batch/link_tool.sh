#!/usr/bin/env bash

if [ ! -n "$3" ]; then
  echo "Data File Linker"
  echo "  Usage: link_tool.sh LEOFS_DIR SHARED_DIR BACKUP_DIR [-i]"
  echo "    LEOFS_DIR : Target LeoFS home directory (ex: /usr/local/leofs/0.16.5)"
  echo "    SHARED_DIR: Common directory of configuration and data files (ex: /usr/local/leofs/shared)"
  echo "    BACKUP_DIR: Backup directory of old configuration and data files (ex: /usr/local/leofs/backup)"
  echo "    -i        : At first use this option. Copy etc, log and work directory"
  echo "                                                    from original directory to shared directory"
  exit 1
fi

LEO_DIR=$1
SHARED_DIR=$2
BACKUP_DIR=$3
DATE=$(date '+%Y%m%d_%H%M%S')

# Check LeoFS home directory exists
if [ ! -d $LEO_DIR ]; then
  echo "ERROR: LeoFS home directory not found"
  exit 1
fi

# Check send directory exists
if [ ! -d $SHARED_DIR ]; then
  mkdir -p $SHARED_DIR
fi

# Check backup directory exists
if [ ! -d $BACKUP_DIR ]; then
  mkdir -p $BACKUP_DIR
fi

# Backup original data
if [ ! -d $LEO_DIR/leo_gateway/org ]; then
  mkdir -p $LEO_DIR/leo_gateway/org
  mv $LEO_DIR/leo_gateway/{etc,log,work} $LEO_DIR/leo_gateway/org
fi
if [ ! -d $LEO_DIR/leo_manager_0/org ]; then
  mkdir -p $LEO_DIR/leo_manager_0/org
  mv $LEO_DIR/leo_manager_0/{etc,log,work} $LEO_DIR/leo_manager_0/org
fi
if [ ! -d $LEO_DIR/leo_manager_1/org ]; then
  mkdir -p $LEO_DIR/leo_manager_1/org
  mv $LEO_DIR/leo_manager_1/{etc,log,work} $LEO_DIR/leo_manager_1/org
fi
if [ ! -d $LEO_DIR/leo_storage/org ]; then
  mkdir -p $LEO_DIR/leo_storage/org
  mv $LEO_DIR/leo_storage/{etc,log,work} $LEO_DIR/leo_storage/org
fi

# Initial copy
if [ "$4" == "-i" ]; then
  if [ ! -d $SHARED_DIR/leo_gateway ]; then
    mkdir -p $SHARED_DIR/leo_gateway
    cp -r $LEO_DIR/leo_gateway/org/* $SHARED_DIR/leo_gateway
  fi
  if [ ! -d $SHARED_DIR/leo_manager_0 ]; then
    mkdir -p $SHARED_DIR/leo_manager_0
    cp -r $LEO_DIR/leo_manager_0/org/* $SHARED_DIR/leo_manager_0
  fi
  if [ ! -d $SHARED_DIR/leo_manager_1 ]; then
    mkdir -p $SHARED_DIR/leo_manager_1
    cp -r $LEO_DIR/leo_manager_1/org/* $SHARED_DIR/leo_manager_1
  fi
  if [ ! -d $SHARED_DIR/leo_storage ]; then
    mkdir -p $SHARED_DIR/leo_storage
    cp -r $LEO_DIR/leo_storage/org/* $SHARED_DIR/leo_storage
  fi
fi

# Link
if [ ! -L $LEO_DIR/leo_gateway/etc ]; then
  ln -s $SHARED_DIR/leo_gateway/etc $LEO_DIR/leo_gateway
fi
if [ ! -L $LEO_DIR/leo_manager_0/etc ]; then
  ln -s $SHARED_DIR/leo_manager_0/etc $LEO_DIR/leo_manager_0
fi
if [ ! -L $LEO_DIR/leo_manager_1/etc ]; then
  ln -s $SHARED_DIR/leo_manager_1/etc $LEO_DIR/leo_manager_1
fi
if [ ! -L $LEO_DIR/$LEO_VER/leo_storage/etc ]; then
  ln -s $SHARED_DIR/leo_storage/etc $LEO_DIR/leo_storage
fi

# Backup old data
mkdir -p $BACKUP_DIR/$DATE
cp -r $SHARED_DIR/leo_* $BACKUP_DIR/$DATE
