
Convert metadata from bitcask to leveldb
========================================

Overview
--------

This tool enable LeoFS administrators to convert metadata used by LeoFS from bitcask to leveldb.

Install dependent libraries
---------------------------
When starting for the first time, you need to issue the below command to install dependent libraries into your local file system

```
make
```

Usage
--------

* Repository:
    * https://github.com/leo-project/leofs_utils/tree/develop/tools/b2l
* Usage:

```
$ ./leofs_b2l -h
Usage: leofs_b2l [-h] [-b <bitcask_dir>] [-l <leveldb_dir>] [-d <debug>] [-v]

  -h, --help            Show the program options
  -b, --bitcask_dir     Specify a bitcask directory to be converted
  -l, --leveldb_dir     Specify a leveldb directory to be outputed
  -d, --debug           Enable debug outputs if specified
  -v, --version         Show version information

### example
$ ./leofs_b2l -b bitcask_dir -l leveldb_dir
```
