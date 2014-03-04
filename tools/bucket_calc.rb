#!/usr/bin/env ruby

require 'optparse'
require 'sqlite3'

class DBUtils
  def initialize(db)
    @db = SQLite3::Database.new($db_fn)
    sql = <<SQL
CREATE TABLE IF NOT EXISTS leofs_keys (
path varchar(1024),
bucket varchar(256),
num integer,
size integer,
unix_t integer,
PRIMARY KEY(path, num)
);
SQL
    @db.execute(sql)
  end

  def db_close
    @db.close
  end

  def transaction_start
    @db.transaction
  end

  def transaction_commit
    @db.commit
  end

  def insert_log(result)
    sql  = "INSERT INTO leofs_keys ("
    sql += "path, "
    sql += "bucket, "
    sql += "num, "
    sql += "size, "
    sql += "unix_t"
    sql += ") VALUES ("
    sql += "\'#{result[2]}\', "
    sql += "\'#{result[1]}\', "
    sql += "#{result[5]}, "
    sql += "#{result[3]}, "
    sql += "#{result[4]}"
    sql += ");"
    @db.execute(sql)
  end

  def update_log(result)
    sql  = "UPDATE leofs_keys SET "
    sql += "size = #{result[3]}, "
    sql += "unix_t = #{result[4]} "
    sql += "WHERE "
    sql += "path = \'#{result[2]}\' AND "
    sql += "num = #{result[5]}"
    sql += ";"
    @db.execute(sql)
  end

  def delete_log(result)
    sql  = "DELETE FROM leofs_keys WHERE "
    sql += "path = \'#{result[2]}\' AND "
    sql += "num = #{result[5]}"
    sql += ";"
    @db.execute(sql)
  end

  def select_log(result)
    sql  = "SELECT "
    sql += "path, "
    sql += "bucket, "
    sql += "num, "
    sql += "size, "
    sql += "unix_t "
    sql += "FROM leofs_keys "
    sql += "WHERE "
    sql += "path = \'#{result[2]}\' AND "
    sql += "num = #{result[5]}"
    sql += ";"
    @db.execute(sql)
  end

  def bucket_list
    sql = "SELECT DISTINCT bucket FROM leofs_keys"
    @db.execute(sql)
  end

  def bucket_size(bucket)
    sql  = "SELECT sum(size) FROM leofs_keys WHERE "
    sql += "bucket = '#{bucket}'"
    @db.execute(sql)
  end

  def bucket_object(bucket)
    sql  = "SELECT count(*) FROM leofs_keys WHERE "
    sql += "bucket = '#{bucket}'"
    @db.get_first_value(sql)
  end
end

class RegistLog
  def initialize(f)
    rows = 0
    insert = 0
    update = 0
    delete = 0
    fp = open(f)
    d = DBUtils.new($db_fn)
    d.transaction_start
    buff_current = fp.gets
    buff_feature = nil
    result = nil
    while true
      buff_feature = fp.gets
      result = devide_line1(buff_current)
      if result[0].nil? then
        unless fp.eof then
          result = devide_line2(buff_current, buff_feature)
          buff_feature = fp.gets
        else
          break
        end
      end
      db_rtn =  d.select_log(result)

      if result[0] == "PUT" then
        if db_rtn.length > 0 then
          if result[4].to_i > db_rtn[0][4] then
            d.update_log(result)
            update += 1
          end
        else
          d.insert_log(result)
          insert += 1
        end
      elsif result[0] == "DELETE" then
        if db_rtn.length > 0 then
          if result[4].to_i >= db_rtn[0][4] then
            d.delete_log(result)
            delete += 1
          end
        end
      end
      buff_current = buff_feature
      rows += 1
    end
    puts "File: #{File.basename(f)} Total: #{rows} Insert: #{insert} Update: #{update} Delete: #{delete}"
    unless fp.closed? then
      fp.close
    end
    d.transaction_commit
    d.db_close
  end

  def devide_line1(l)
    /\[([^\t]+)\]\t(.+)\t(.+)\t(.+)\t.+\t(.+)\t.+/ =~ l
    return [$1, $2, $3, $4, $5, 0]
  end

  def devide_line2(l, m)
    l += m
    l.gsub!("\n", "\t")
    /\[([^\t]+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t.+\t(.+)\t.+/ =~ l
    return [$1, $2, $3, $5, $6, $4]
  end
end

class AnalyzeLog
  def initialize
    d = DBUtils.new($db_fn)
    b_list = d.bucket_list[0]
    b_list.length.times do |i|
      b_size = d.bucket_size(b_list[i])
      b_obj = d.bucket_object(b_list[i])
      puts "Bucket: #{b_list[i]} Object: #{b_obj} Size: #{b_size[0][0] / 1024 / 1024 / 1024}GB"
    end
  end
end

$db_fn = "leofs_access_log.db"
$mode = "regist"
o = OptionParser.new
o.banner = "Usage : #{__FILE__} [log_file1]..[log_fileN] [-o database_file] | [-p]"
o.on('-o', '--db_file=database_file', 'default=leofs_access_log.db') {|v| $db_fn = v}
o.on('-p','--print', 'print stored object size') {|v| $mode = "calc"}
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
  t = Time.now
  puts "Start: " + t.strftime("%Y-%m-%d %H:%M:%S")
  ARGV.length.times do |i|
    RegistLog.new(ARGV[i])
  end
  puts "End  : " + t.strftime("%Y-%m-%d %H:%M:%S")
else
  AnalyzeLog.new
end
