#!/usr/bin/env ruby

require 'optparse'
require 'mysql2'

class DBUtils
  def initialize()
    begin
      @db = Mysql2::Client::new(:host=>$host, :username=>$user, :password=>$pass, :database=>"leofs")
    rescue => e
      puts "[ERROR] Can't connect DB.\n#{e.to_s}"
      exit
    end

    # for test
    if $init then
      sql  = "DROP TABLE leofs_keys"
      @db.query(sql)
    end

    sql  = "CREATE TABLE IF NOT EXISTS leofs_keys ("
    #sql += "id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,"
    sql += "bucket VARCHAR(1000) NOT NULL,"
    sql += "path VARCHAR(1000) NOT NULL,"
    sql += "size BIGINT UNSIGNED NOT NULL,"
    sql += "unix_t BIGINT UNSIGNED NOT NULL,"
    sql += "PRIMARY KEY(path)"
    sql += ") ENGINE = MYISAM"

    begin
      @db.query(sql)
    rescue => e
      puts "[ERROR] Can't create table.\n#{e.to_s}"
      exit
    end
  end

  def db_close
    @db.close
  end

  def transaction_start
    @db.query("START TRANSACTION")
  end

  def transaction_commit
    @db.query("COMMIT")
  end

  def transaction_rollback
    @db.query("ROLLBACK")
  end

  def insert_log(result)
    sql  = "INSERT INTO leofs_keys ("
    sql += "path, "
    sql += "bucket, "
    sql += "size, "
    sql += "unix_t"
    sql += ") VALUES ("
    sql += "\'#{result[2]}\', "
    sql += "\'#{result[1]}\', "
    sql += "#{result[4]}, "
    sql += "#{result[5]}"
    sql += ")"
    @db.query(sql)
  end

  def update_log(result)
    sql  = "UPDATE leofs_keys SET "
    sql += "size = #{result[4]}, "
    sql += "unix_t = #{result[5]} "
    sql += "WHERE "
    sql += "path = \'#{result[2]}\'"
    @db.query(sql)
  end

  def update_add_log(result)
    sql  = "UPDATE leofs_keys SET "
    sql += "size = size + #{result[4]}, "
    sql += "unix_t = #{result[5]} "
    sql += "WHERE "
    sql += "path = \'#{result[2]}\'"
    @db.query(sql)
  end

  def delete_log(result)
    sql  = "DELETE FROM leofs_keys WHERE "
    sql += "path = \'#{result[2]}\'"
    @db.query(sql)
  end

  def select_log(result)
    sql  = "SELECT "
    #sql += "path, "
    #sql += "bucket, "
    #sql += "size, "
    sql += "unix_t "
    sql += "FROM leofs_keys "
    sql += "WHERE "
    sql += "path = \'#{result[2]}\'"
    @db.query(sql)
  end

  def bucket_list
    sql = "SELECT DISTINCT bucket FROM leofs_keys"
    @db.query(sql)
  end

  def bucket_size(bucket)
    sql  = "SELECT sum(size) FROM leofs_keys WHERE "
    sql += "bucket = '#{bucket}'"
    @db.query(sql)
  end

  def bucket_object(bucket)
    sql  = "SELECT count(*) FROM leofs_keys WHERE "
    sql += "bucket = '#{bucket}'"
    @db.query(sql)
  end
end

class RegistLog
  def initialize(f)
    rows = 0
    insert = 0
    update = 0
    delete = 0
    buff_feature = nil
    result = nil
    unix_t = 0
    begin
      fp = open(f)
      d = DBUtils.new()
      #d.transaction_start
      until fp.eof? do
        buff = fp.gets
        result = devide_line(buff)
        if result[0].nil? then
          raise "Data is wrong."
        end
        db_rtn =  d.select_log(result)
        if result[0] == "PUT" then
          if db_rtn.size > 0 then
            db_rtn.each do |row|
              unix_t = row["unix_t"]
            end
            if result[5].to_i > unix_t then
              if result[3].to_i <= 1 then
                d.update_log(result)
                update += 1
              else
                d.update_add_log(result)
              end
            end
          else
            d.insert_log(result)
            insert += 1
          end
        elsif result[0] == "DELETE" then
          if db_rtn.size > 0 then
            db_rtn.each do |row|
              unix_t = row["unix_t"]
            end
            if result[5].to_i >= unix_t then
              d.delete_log(result)
              delete += 1
            end
          end
        end
        rows += 1
        if rows % 1000 == 0 then
          print "."
        end
      end
      puts "\nFile: #{File.basename(f)} Total: #{rows} Insert: #{insert} Update: #{update} Delete: #{delete}"
      #d.transaction_commit
      unless fp.closed? then
        fp.close
      end
      d.db_close
    rescue => e
      #d.transaction_rollback
      puts "[ERROR] error.\n#{e.to_s}"
      exit
    end
  end

  def devide_line(l)
    /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t.+\t(\d{16})\t.+/ =~ l
    return [$1, $2, $3, $4.to_i, $5.to_i, $6]
  end
end

class AnalyzeLog
  def initialize
    obj = ''
    size = 0
    puts "+------------------------------------------------+--------------------+--------------------+"
    puts "|                     Bucket                     |       Object       |       Size(GB)     |"
    puts "+------------------------------------------------+--------------------+--------------------+"
    d = DBUtils.new()
    b_list = d.bucket_list
    b_list.each do |row|
      b_obj = d.bucket_object(row["bucket"])
      b_obj.each do |o|
        obj = o["count(*)"]
      end
      b_size = d.bucket_size(row["bucket"])
      b_size.each do |s|
        size = s["sum(size)"] / 1024 / 1024 / 1024
      end
      puts "|%-48s|%20d|%20d|"%[row["bucket"], obj, size]
    end
    puts "+------------------------------------------------+--------------------+--------------------+"
  end
end

$host = "127.0.0.1"
$user = ""
$pass = ""
$mode = "regist"
$init = false

o = OptionParser.new
o.banner = "Usage : #{__FILE__} [log_file1]..[log_fileN] [-h mysql_hostname] [-u mysql_username] [-p mysql_password] | [-s]"
o.on('-h', '--host=mysql_hostname', 'default=127.0.0.1') {|v| $host = v}
o.on('-u', '--user=mysql_username') {|v| $user = v}
o.on('-p', '--pass=mysql_password') {|v| $pass = v}
o.on('-s', '--size', 'print stored object size') {|v| $mode = "calc"}
o.on('--initialize', '!!ALL RECORDS DELETE!!'){|v| $init = true}
begin
  o.parse!
rescue
  puts "ERROR : unrecognized option"
  STDERR.puts o.help
  exit
end

if $mode == "regist" && ARGV.length == 0 then
  puts "ERROR : unrecognized option"
  STDERR.puts o.help
  exit
end

if $mode == "regist" then
  #t = Time.now
  puts "Start: " + Time.now.strftime("%Y-%m-%d %H:%M:%S")
  ARGV.length.times do |i|
    RegistLog.new(ARGV[i])
  end
  puts "End  : " + Time.now.strftime("%Y-%m-%d %H:%M:%S")
else
  AnalyzeLog.new
end

exit
