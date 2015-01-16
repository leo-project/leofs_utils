
Merge two leveldb files
========================================

Overview
--------

This tool enable LeoFS administrators to merge two leveldb metadata used by LeoFS.

Install dependent libraries
---------------------------
When starting for the first time, you need to issue the below command to install dependent libraries into your local file system.

```
make
```

Usage
--------

* Repository:
    * https://github.com/leo-project/leofs_utils/tree/develop/tools/l2l
* Usage:

```
$ ./leofs_l2l.erl -h
Usage: leofs_l2l.erl [-h] [-s <src_dir>] [-d <dst_dir>] [-v]

  -h, --help            Show the program options
  -s, --bitcask_dir     Specify a bitcask directory to be converted
  -d, --leveldb_dir     Specify a leveldb directory to be outputed
  -v, --version         Show version information

### example
$ ./leofs_l2l -b leveldb_src_dir -l leveldb_dst_dir
```
