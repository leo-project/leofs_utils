leofs_utils
===========

## Capistrano3 receipe for LeoFS
### Preparations in advance
1. Install Ruby 2.x
>
```
    Recommend to install rbenv
```

1. Install bundler
1. Install capistrano
```
    $ rbenv exec gem install capistrano
```

1. Edit config/deploy.rb (Please review it and modify as needed.)
```ruby
    # Install Operator
    set :op_user,             "op_user"

    # Receipe Directory
    set :home,                "/home/op_user/leofs-utils/capistrano"    

    # LeoFS group
    set :leofs_group,         "leofs"

    #LeoFS gid
    set :leofs_gid,           "1000"

    # LeoFS user
    set :leofs_user,          "leofs"

    # LeoFS user's password
    set :leofs_passwd,        "leofs"

    # LeoFS home directory
    set :leofs_home,          "/usr/local/leofs"

    # Source file store location (all server)
    set :src_dir,             "/home/#{fetch(:op_user)}/src"

    # LeoFS rpm file name
    set :rpm_file,            "leofs-1.0.0-pre1.x86_64.rpm"

    # LeoFS rpm file name for upload (full path)
    set :src_rpm_file,        "#{fetch(:home)}/rpm/#{fetch(:rpm_file)}"

    # LeoFS Utility files store location
    set :src_utils_dir,       "#{fetch(:home)}/leofs_utils"

    # Utility directory name
    set :utils_dir,           "leofs_utils"

    # Utility files store location
    set :bin_dir,             "/home/#{fetch(:op_user)}/bin"

    # LeoFS shared directory name
    set :shared_dir,          "shared"

    # LeoFS backup directory name
    set :backup_dir,          "backup"

    # LeoFS primary manager name
    set :leo_manager_0,       "manager_0@127.0.0.1"

    # LeoFS secondary manager name
    set :leo_manager_1,       "manager_1@127.0.0.1"

    # LeoFS num_of_replicas
    set :num_of_replicas,     "3"

    # LeoFS consistency write
    set :consistency_write,   "2"

    # LeoFS consistency read
    set :consistency_read,    "1"

    # LeoFS consistency delete
    set :consistency_delete,  "2"

    # LeoFS avs directory location
    set :leo_storage_avs_dir, "/avs"

    # LeoFS avs file separate number
    set :leo_storage_avs_num, "64"
```

1. Edit config/deploy/leofs.rb (Please review it and modify as needed.)
```ruby
    server '127.0.0.1', user: 'leofs', roles: %w{manager_0}
    server '127.0.0.1', user: 'leofs', roles: %w{manager_1}
    server '127.0.0.1', user: 'leofs', roles: %w{gateway}
    server '127.0.0.1', user: 'leofs', roles: %w{storage}
```

1. Clone leofs_utils repository
```sh
$ git clone https://github.com/firejun/leofs_utils.git
```

1. Place LeoFS rpm file under 'rpm' directory

### Command List
+ Create LeoFS user
```
$ bundle exec cap leofs create_user
```

+ Delete LeoFS user
```
$ bundle exec cap leofs delete_user
```

+ Install rpm package
```
$ bundle exec cap leofs install_rpm
```

+ Uninstall rpm package
```
$ bundle exec cap leofs uninstall_rpm
```

+ Install utils package
```
$ bundle exec cap leofs install_utils
```

+ Uninstall utils package
```
$ bundle exec cap leofs install_utils
```

+ Exec link_tool
```
$ bundle exec cap leofs exec_link_tool
```

+ Configure manager_0
```
bundle exec cap leofs configure_manager_0
```

+ Configure manager_1
```
bundle exec cap leofs configure_manager_1
```

+ Configure storage
```
bundle exec cap leofs configure_storage
```

+ Configure gateway
```
bundle exec cap leofs configure_gateway
```

+ Start manager_0
```
bundle exec cap leofs start_manager_0
```

+ Stop manager_0
```
bundle exec cap leofs stop_manager_0
```

+ Start manager_1
```
bundle exec cap leofs start_manager_1
```

+ Stop manager_1
```
bundle exec cap leofs stop_manager_1
```

+ Start storage
```
bundle exec cap leofs start_storage
```

+ Resume storage
```
bundle exec cap leofs resume_storage
```

+ Stop storage
```
bundle exec cap leofs stop_storage
```

+ Start gateway
```
bundle exec cap leofs start_gateway
```

+ Stop manager_1
```
bundle exec cap leofs stop_gateway
```

+ Reset Data
```
bundle exec cap leofs reset_data
```

### For Example
1. First deploy

    ```
    $ bundle exec cap leofs create_user
    $ bundle exec cap leofs install_rpm
    $ bundle exec cap leofs install_utils
    $ bundle exec cap leofs exec_link_tool
    $ bundle exec cap leofs configure_manager_0
    $ bundle exec cap leofs configure_manager_1
    $ bundle exec cap leofs configure_storage
    $ bundle exec cap leofs configure_gateway
    $ bundle exec cap leofs start_manager_0
    $ bundle exec cap leofs start_manager_1
    $ bundle exec cap leofs start_storage
    $ bundle exec cap leofs start_gateway
    ```

