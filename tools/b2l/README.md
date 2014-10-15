
Convert metadata from bitcask to leveldb
========================================

Overview
--------

This tool enable LeoFS administrators to convert metadata used by LeoFS from bitcask to leveldb.

Usage
--------

* Repository:
    * https://github.com/leo-project/leofs_utils/tree/develop/tools/b2l
* Usage:

```
$ ./leofs_b2l -h
Usage: leofs_b2l [-h] [-b <bitcask_dir>] [-l <leveldb_dir>] [-v]

  -h, --help            Show the program options
  -b, --bitcask_dir     Specify a bitcask directory to be converted
  -l, --leveldb_dir     Specify a leveldb directory to be outputed
  -v, --version         Show version information

### example
$ ./leofs_b2l -b bitcask_dir -l leveldb_dir
```
