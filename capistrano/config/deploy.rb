# config valid only for Capistrano 3.1
lock '3.1.0'

# set :application, 'my_app_name'
# set :repo_url, 'git@example.com:me/my_repo.git'

# Default branch is :master
# ask :branch, proc { `git rev-parse --abbrev-ref HEAD`.chomp }

# Default deploy_to directory is /var/www/my_app
# set :deploy_to, '/var/www/my_app'

# Default value for :scm is :git
# set :scm, :git

# Default value for :format is :pretty
# set :format, :pretty

# Default value for :log_level is :debug
# set :log_level, :debug

# Default value for :pty is false
# set :pty, true
set :pty, true

# Default value for :linked_files is []
# set :linked_files, %w{config/database.yml}

# Default value for linked_dirs is []
# set :linked_dirs, %w{bin log tmp/pids tmp/cache tmp/sockets vendor/bundle public/system}

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for keep_releases is 5
# set :keep_releases, 5

set :ssh_options, { :forward_agent => true }

########################################################################################################

## LeoFS environment

# Install Operator ID
set :op_user,             "op_user"

# Receipe Directory
set :home,                "/home/op_user/leofs-utils/capistrano"

# LeoFS group
set :leofs_group,         "leofs"

#LeoFS group ID
set :leofs_gid,           "1000"

# LeoFS user ID
set :leofs_user,          "leofs"

# LeoFS user's password -:P
# TODO: change to id_rsa
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

########################################################################################################

#namespace :deploy do
  desc 'Create leofs user'
  task :create_user do
    on roles(:all) do
      ret = capture "getent group | grep #{fetch(:leofs_group)} | wc -l"
      if ret == "0" then
        execute :sudo, "groupadd -g #{fetch(:leofs_gid)} #{fetch(:leofs_group)}"
      end
      ret = capture "getent passwd | grep #{fetch(:leofs_user)} | wc -l"
      if ret == "0" then
        execute :sudo, "useradd -g #{(fetch(:leofs_gid)} #{fetch(:leofs_user)}"
        execute "echo -e \"#{fetch(:leofs_user)}:#{fetch(:leofs_passwd)}\" | sudo chpasswd"
      end
    end
  end

  desc 'Delete leofs user'
  task :delete_user do
    on roles(:all) do
      ret = capture "getent passwd | grep #{fetch(:leofs_user)} | wc -l"
      if ret == "1" then
        execute :sudo, "userdel -r #{fetch(:leofs_user)}"
      end
    end
  end

  desc 'Install rpm package'
  task :install_rpm do
    on roles(:all) do
      leofs_ver = File.basename(fetch(:rpm_file), ".x86_64.rpm")
      leofs_ver.gsub!("leofs-", "")
      if test "[ ! -d #{fetch(:leofs_home)} ]" then
        execute :sudo, :mkdir, '-p', "#{fetch(:leofs_home)}"
        execute :sudo, "chown -R #{fetch(:leofs_user)}:#{fetch(:leofs_group)} #{fetch(:leofs_home)}"
      end
      if test "[ ! -d #{fetch(:src_dir)} ]" then
        execute :mkdir, '-p', "#{fetch(:src_dir)}"
      end
      #if test "[ ! -f #{fetch(:src_dir)}/#{fetch(:rpm_file)} ]" then
        upload!("#{fetch(:src_rpm_file)}", "#{fetch(:src_dir)}/#{fetch(:rpm_file)}")
      #end
      execute :sudo, "rpm -ivh #{fetch(:src_dir)}/#{fetch(:rpm_file)}"
      execute :sudo, "chown -R #{fetch(:leofs_user)}:#{fetch(:leofs_group)} #{fetch(:leofs_home)}/#{leofs_ver}"
      execute :sudo, "ln -s #{fetch(:leofs_home)}/#{leofs_ver} #{fetch(:leofs_home)}/current"
      execute :sudo, "chown -R #{fetch(:leofs_user)}:#{fetch(:leofs_group)} #{fetch(:leofs_home)}/current"
    end
  end

  desc 'Uninstall rpm package'
  task :uninstall_rpm do
    on roles(:all) do
      rpm_nm = File.basename(fetch(:rpm_file), ".rpm")
      leofs_ver = File.basename(fetch(:rpm_file), ".x86_64.rpm")
      leofs_ver.gsub!("leofs-", "")
      ret = capture :sudo, "rpm -qa | grep #{rpm_nm} | wc -l"
      if ret == "1" then
        execute :sudo, "rpm -e #{rpm_nm}"
        execute :sudo, "rm -rf #{fetch(:leofs_home)}/#{leofs_ver}"
      end
    end
  end

  desc 'Install utils package'
  task :install_utils do
    on roles(:all) do
      if test "[ ! -d #{fetch(:bin_dir)} ]" then
        execute :mkdir, '-p', "#{fetch(:bin_dir)}"
      end
      if test "[ ! -d #{fetch(:bin_dir)}/#{fetch(:utils_dir)} ]" then
        upload!("#{fetch(:src_utils_dir)}", "#{fetch(:bin_dir)}", :recursive => true)
      end
    end
  end

  desc 'Uninstall utils package'
  task :uninstall_utils do
    on roles(:all) do
      if test "[ -d #{fetch(:bin_dir)}/#{fetch(:utils_dir)} ]" then
        execute "rm -rf #{fetch(:bin_dir)}/#{fetch(:utils_dir)}"
      end
    end
  end

  desc 'Exec link_tool'
  task :exec_link_tool do
    on roles(:all) do
      leofs_ver = File.basename(fetch(:rpm_file), ".x86_64.rpm")
      leofs_ver.gsub!("leofs-", "")
      if test "[ -d #{fetch(:bin_dir)}/#{fetch(:utils_dir)} ]" then
        execute :sudo, "sh #{fetch(:bin_dir)}/#{fetch(:utils_dir)}/batch/link_tool.sh -i #{fetch(:leofs_home)}/#{leofs_ver} #{fetch(:leofs_home)}/#{fetch(:shared_dir)} #{fetch(:leofs_home)}/#{fetch(:backup_dir)}"
        execute :sudo, "chown -R #{fetch(:leofs_user)}:#{fetch(:leofs_group)} #{fetch(:leofs_home)}/#{fetch(:shared_dir)} #{fetch(:leofs_home)}/#{fetch(:backup_dir)} #{fetch(:leofs_home)}/#{leofs_ver}"
      end
    end
  end

  desc 'Configure_Manager_0'
  task :configure_manager_0 do
    on roles(:manager_0) do
      leofs_dir = "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}"
      leofs_dir.gsub!('/', '\\\/')
      hostname = capture "hostname -s"
      ip_address = capture "hostname -i"
      if test "[ ! -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.org ]" then
        if test "[ -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf ]" then
          execute :sudo, :cp, "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf", "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.org"
          execute :sudo, "chown leofs:leofs #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.org"
        end
      else
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.sasl_error_log.*/sasl\\\.sasl_error_log = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/sasl\\\/sasl-error.log/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.org > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.error_logger_mf_dir.*/sasl\\\.error_logger_mf_dir = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/sasl/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*manager\\\.partner.*/manager\\\.partner = #{fetch(:leo_manager_1)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.num_of_replicas.*/consistency\\\.num_of_replicas = #{fetch(:num_of_replicas)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.write.*/consistency\\\.write = #{fetch(:consistency_write)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.read.*/consistency\\\.read = #{fetch(:consistency_read)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.delete.*/consistency\\\.delete = #{fetch(:consistency_delete)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*mnesia\\\.dir.*/mnesia\\\.dir = #{leofs_dir}\\\/leo_manager_0\\\/work\\\/mnesia\\\/#{ip_address}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.erlang.*/log\\\.erlang = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/erlang/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.app.*/log\\\.app = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/app/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.member_dir.*/log\\\.member_dir = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.ring_dir.*/log\\\.ring_dir = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*queue_dir.*/queue_dir = #{leofs_dir}\\\/leo_manager_0\\\/work\\\/queue/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*nodename.*/nodename = #{hostname}@#{ip_address}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*erlang\\\.crash_dump.*/erlang\\\.crash_dump = #{leofs_dir}\\\/leo_manager_0\\\/log\\\/erl_crash\\\.dump/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf\""
        execute :sudo, "rm -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.0 #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf.tmp.1"
        execute :sudo, "chown #{fetch(:leofs_group)}:#{fetch(:leofs_user)} #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/etc/leo_manager.conf"
      end
    end
  end

  desc 'Configure_Manager_1'
  task :configure_manager_1 do
    on roles(:manager_1) do
      leofs_dir = "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}"
      leofs_dir.gsub!('/', '\\\/')
      hostname = capture "hostname -s"
      ip_address = capture "hostname -i"
      if test "[ ! -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.org ]" then
        if test "[ -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf ]" then
          execute :sudo, :cp, "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf", "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.org"
          execute :sudo, "chown leofs:leofs #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.org"
        end
      else
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.sasl_error_log.*/sasl\\\.sasl_error_log = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/sasl\\\/sasl-error.log/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.org > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.error_logger_mf_dir.*/sasl\\\.error_logger_mf_dir = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/sasl/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*manager\\\.partner.*/manager\\\.partner = #{fetch(:leo_manager_0)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.num_of_replicas.*/consistency\\\.num_of_replicas = #{fetch(:num_of_replicas)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.write.*/consistency\\\.write = #{fetch(:consistency_write)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.read.*/consistency\\\.read = #{(:consistency_read)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*consistency\\\.delete.*/consistency\\\.delete = #{(:consistency_delete)}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*mnesia\\\.dir.*/mnesia\\\.dir = #{leofs_dir}\\\/leo_manager_1\\\/work\\\/mnesia\\\/#{ip_address}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.erlang.*/log\\\.erlang = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/erlang/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.app.*/log\\\.app = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/app/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.member_dir.*/log\\\.member_dir = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.ring_dir.*/log\\\.ring_dir = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*queue_dir.*/queue_dir = #{leofs_dir}\\\/leo_manager_1\\\/work\\\/queue/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*nodename.*/nodename = #{hostname}@#{ip_address}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*erlang\\\.crash_dump.*/erlang\\\.crash_dump = #{leofs_dir}\\\/leo_manager_1\\\/log\\\/erl_crash\\\.dump/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf\""
        execute :sudo, "rm -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.0 #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf.tmp.1"
        execute :sudo, "chown #{fetch(:leofs_group)}:#{fetch(:leofs_user)} #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/etc/leo_manager.conf"
      end
    end
  end

  desc 'Configure Storage'
  task :configure_storage do
    on roles(:storage) do
      leofs_dir = "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}"
      leofs_dir.gsub!('/', '\\\/')
      avs_dir = "#{fetch(:leo_storage_avs_dir)}"
      avs_dir.gsub!('/', '\\\/')
      hostname = capture "hostname -s"
      ip_address = capture "hostname -i"
      if test "[ ! -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.org ]" then
        if test "[ -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf ]" then
          execute :sudo, :cp, "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf", "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.org"
          execute :sudo, "chown leofs:leofs #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.org"
        end
      end
      else
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.sasl_error_log.*/sasl\\\.sasl_error_log = #{leofs_dir}\\\/leo_storage\\\/log\\\/sasl\\\/sasl-error.log/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.org > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.error_logger_mf_dir.*/sasl\\\.error_logger_mf_dir = #{leofs_dir}\\\/leo_storage\\\/log\\\/sasl/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*managers.*/managers = [#{fetch(:leo_manager_0)}, #{fetch(:leo_manager_1)}]/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*obj_containers\\\.path = \\\[\\\.\\\/avs\\\].*/obj_containers\\\.path = \\\[#{avs_dir}\\\]/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*obj_containers\\\.num_of_containers = \\\[8\\\]$/obj_containers\\\.num_of_containers = \\\[#{fetch(:leo_storage_avs_num)}\\\]/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.erlang.*/log\\\.erlang = #{leofs_dir}\\\/leo_storage\\\/log\\\/erlang/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.app.*/log\\\.app = #{leofs_dir}\\\/leo_storage\\\/log\\\/app/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.member_dir.*/log\\\.member_dir = #{leofs_dir}\\\/leo_storage\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.ring_dir.*/log\\\.ring_dir = #{leofs_dir}\\\/leo_storage\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*queue_dir.*/queue_dir = #{leofs_dir}\\\/leo_storage\\\/work\\\/queue/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*nodename.*/nodename = #{hostname}@#{ip_address}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*erlang\\\.crash_dump.*/erlang\\\.crash_dump = #{leofs_dir}\\\/leo_storage\\\/log\\\/erl_crash.dump/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf\""
        execute :sudo, "rm -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.0 #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf.tmp.1"
        execute :sudo, "chown #{fetch(:leofs_group)}:#{fetch(:leofs_user)} #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/etc/leo_storage.conf"
      end
    end
  end

  desc 'Configure Gateway'
  task :configure_gateway do
    on roles(:gateway) do
      leofs_dir = "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}"
      leofs_dir.gsub!('/', '\\\/')
      hostname = capture "hostname -s"
      ip_address = capture "hostname -i"
      if test "[ ! -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.org ]" then
        if test "[ -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf ]" then
          execute :sudo, :cp, "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf", "#{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.org"
          execute :sudo, "chown leofs:leofs #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.org"
        end
      else
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.sasl_error_log.*/sasl\\\.sasl_error_log = #{leofs_dir}\\\/leo_gateway\\\/log\\\/sasl\\\/sasl-error.log/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.org > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*sasl\\\.error_logger_mf_dir.*/sasl\\\.error_logger_mf_dir = #{leofs_dir}\\\/leo_gateway\\\/log\\\/sasl/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*managers.*/managers = [#{fetch(:leo_manager_0)}, #{fetch(:leo_manager_1)}]/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*http\\\.ssl_certfile.*/http\\\.ssl_certfile = #{leofs_dir}\\\/leo_gateway\\\/etc\\\/server_cert\\\.pem/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*http\\\.ssl_keyfile.*/http\\\.ssl_keyfile = #{leofs_dir}\\\/leo_gateway\\\/etc\\\/server_key\\\.pem/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.erlang.*/log\\\.erlang = #{leofs_dir}\\\/leo_gateway\\\/log\\\/erlang/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.app.*/log\\\.app = #{leofs_dir}\\\/leo_gateway\\\/log\\\/app/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.member_dir.*/log\\\.member_dir = #{leofs_dir}\\\/leo_gateway\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*log\\\.ring_dir.*/log\\\.ring_dir = #{leofs_dir}\\\/leo_gateway\\\/log\\\/ring/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*queue_dir.*/queue_dir = #{leofs_dir}\\\/leo_gateway\\\/work\\\/queue/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*nodename.*/nodename = #{hostname}@#{ip_address}/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0\""
        execute :sudo, "sh -c \"sed -e \\\"s\/^.*erlang\\\.crash_dump.*/erlang\\\.crash_dump = #{leofs_dir}\\\/leo_gateway\\\/log\\\/erl_crash.dump/\\\" #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 > #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf\""
        execute :sudo, "rm -f #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.0 #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf.tmp.1"
        execute :sudo, "chown #{fetch(:leofs_group)}:#{fetch(:leofs_user)} #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/etc/leo_gateway.conf"
      end
    end
  end

  desc 'Start manager_0'
  task :start_manager_0 do
    on roles(:manager_0) do |x|
      if test "[ -f #{fetch(:leofs_home)}/current/leo_manager_0/bin/leo_manager ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_manager_0/bin/leo_manager ping; true"
        if ret != "pong" then
          execute :sudo, "-u leofs nohup /usr/local/leofs/current/leo_manager_0/bin/leo_manager start >/dev/null 2>&1 < /dev/null"
        end
      end
    end
  end

  desc 'Stop manager_0'
  task :stop_manager_0 do
    on roles(:manager_0) do
      if test "[ -f #{fetch(:leofs_home)}/current/leo_manager_0/bin/leo_manager ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_manager_0/bin/leo_manager ping; true"
        if ret == "pong" then
          execute :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_manager_0/bin/leo_manager stop"
          execute :sudo, "-u leofs killall epmd"
        end
      end
    end
  end

  desc 'Start manager_1'
  task :start_manager_1 do
    on roles(:manager_1) do |x|
      if test "[ -f #{fetch(:leofs_home)}/current/leo_manager_1/bin/leo_manager ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_manager_1/bin/leo_manager ping; true"
        if ret != "pong" then
          execute :sudo, "-u leofs nohup /usr/local/leofs/current/leo_manager_1/bin/leo_manager start >/dev/null 2>&1 < /dev/null"
        end
      end
    end
  end

  desc 'Stop manager_1'
  task :stop_manager_1 do
    on roles(:manager_1) do
      if test "[ -f #{fetch(:leofs_home)}/current/leo_manager_1/bin/leo_manager ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_manager_1/bin/leo_manager ping; true"
        if ret == "pong" then
          execute :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_manager_1/bin/leo_manager stop"
          execute :sudo, "-u leofs killall epmd"
        end
      end
    end
  end

  desc 'Start storage'
  task :start_storage do
    on roles(:storage) do |x|
      if test "[ -f #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage ping; true"
        if ret != "pong" then
          execute :sudo, "-u leofs nohup /usr/local/leofs/current/leo_storage/bin/leo_storage start >/dev/null 2>&1 < /dev/null"
        end
      end
    end
  end

  desc 'Resume storage'
  task :resume_storage do
    on roles(:storage) do |x|
      if test "[ -f #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage ping; true"
        if ret == "pong" then
          manager_0 = "#{fetch(:leo_manager_0)}".split("@")[1]
          host = capture "hostname"
          execute "echo \"resume #{host}@#{x.hostname}\" | nc #{manager_0} 10010"
        end
      end
    end
  end

  desc 'Stop storage'
  task :stop_storage do
    on roles(:storage) do |x|
      if test "[ -f #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage ping; true"
        if ret == "pong" then
          manager_0 = "#{fetch(:leo_manager_0)}".split("@")[1]
          host = capture "hostname"
          execute "echo \"suspend #{host}@#{x.hostname}\" | nc #{manager_0} 10010"
          execute :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_storage/bin/leo_storage stop"
          execute :sudo, "-u leofs killall epmd"
        end
      end
    end
  end

  desc 'Start gateway'
  task :start_gateway do
    on roles(:gateway) do |x|
      if test "[ -f #{fetch(:leofs_home)}/current/leo_gateway/bin/leo_gateway ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_gateway/bin/leo_gateway ping; true"
        if ret != "pong" then
          execute :sudo, "-u leofs nohup /usr/local/leofs/current/leo_gateway/bin/leo_gateway start >/dev/null 2>&1 < /dev/null"
        end
      end
    end
  end

  desc 'Stop gateway'
  task :stop_gateway do
    on roles(:gateway) do
      if test "[ -f #{fetch(:leofs_home)}/current/leo_gateway/bin/leo_gateway ]" then
        ret = capture :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_gateway/bin/leo_gateway ping; true"
        if ret == "pong" then
          execute :sudo, "-u leofs #{fetch(:leofs_home)}/current/leo_gateway/bin/leo_gateway stop"
          execute :sudo, "-u leofs killall epmd"
        end
      end
    end
  end

  desc 'Reset Data'
  task :reset_data do
    on roles(:manager_0) do
      if test "[ -d #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/work ]" then
        execute :sudo, "-u leofs rm -rf #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/work/mnesia/* #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_0/work/queue/*"
      end
    end
    on roles(:manager_1) do
      if test "[ -d #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/work ]" then
        execute :sudo, "-u leofs rm -rf #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/work/mnesia/* #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_manager_1/work/queue/*"
      end
    end
    on roles(:storage) do
      if test "[ -d #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/work ]" then
        execute :sudo, "-u leofs rm -rf #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/work/mnesia/* #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_storage/work/queue/*"
      end
    end
    on roles(:gateway) do
      if test "[ -d #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/work ]" then
        execute :sudo, "-u leofs rm -rf #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/work/mnesia/* #{fetch(:leofs_home)}/#{fetch(:shared_dir)}/leo_gateway/work/queue/*"
      end
    end
  end
#end
