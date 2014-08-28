
Vacuum records in command history table
=======================================

Overview
--------

This tool enable LeoFS administrators to vacuum records in command history table.
This is a temporary solution against #225 until the fundamental fix will be released.

Usage
--------

* Repository:
    * https://github.com/leo-project/leofs_utils/tree/develop/tools/vacuum_history
* Usage:

```
$ ./leofs_vacuum_history -h
Usage: leofs_vacuum_history [-h] [-m <manager>] [-c <cookie>]
                           [-r <record>] [-v]

  -h, --help        Show the program options
  -m, --manager     Specify a manager node to connect
  -c, --cookie      Specify a cookie to connect
  -r, --record      The number of record to be kept
  -v, --version     Show version information

### Vacuum records in command history table to be
$ ./leofs_vacuum_history -m manager_0@127.0.0.1 -c 401321b4 -r 100
```
