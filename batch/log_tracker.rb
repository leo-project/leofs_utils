#!/usr/bin/env ruby
require 'optparse'

class ReadLog
  def initialize(fn)
    @f = open(fn)
  end

  def read_line
    begin
      @f.gets
    rescue
      unless @f.closed?
        @f.close
      end
    end
  end

  def close_file
    unless @f.closed?
      @f.close
    end
  end
end

class WriteLog
  def initialize(fn)
    if fn == $stdout then
      @f = $stdout
    else
      @f = open(fn, "w")
    end
  end

  def write_line(l)
    @f.write(l + "\n")
  end

  def close_file
    unless @f.closed?
      @f.close
    end
  end
end

class BucketLog
  attr_accessor :bn, :head_cnt, :put_cnt, :get_cnt, :del_cnt, :cap, :fn

  def initialize(bn, fn, dt)
    @bn = bn
    @fn = fn
    @dt = dt
    @head_cnt = 0
    @put_cnt = 0
    @get_cnt = 0
    @del_cnt = 0
    @cap = 0
    @max_cnt = 0
    @prev_dt = "0"
    @work_cnt = 0
  end

  def b_sum(type, cap, dt)
    if type == "HEAD" then
      @head_cnt += 1
    elsif type == "PUT" then
      @put_cnt += 1
      @cap += cap.to_i
    elsif type == "GET" then
      @get_cnt += 1
    elsif type == "DELETE" then
      @del_cnt += 1
      @cap -= cap.to_i
    end

    if @prev_dt == dt then
      @work_cnt += 1
    else
      @work_cnt = 1
      @prev_dt = dt
    end
    if @max_cnt < @work_cnt then
      @max_cnt = @work_cnt
    end
  end

  def get_max
    (@max_cnt / 10.0).round(1)
  end

  def get_dt
    @dt[0,4] + "-" + @dt[4,2] + "-" + @dt[6,2] + " " + @dt[8,2] + ":00:00"
  end

  def get_avg
    ((@head_cnt + @put_cnt + @get_cnt + @del_cnt) / 3600.0).round(1)
  end
end

$save_fn = $stdout
o = OptionParser.new
o.banner = "Usage : #{__FILE__} [log_file1]..[log_fileN] [-o output_file]"
o.on('-o output_file', '[output file name]', '(default=STDOUT)') {|v| $save_fn = v }
begin
  o.parse!
rescue
  STDERR.puts o.help
  puts "ERROR : unrecognized option"
  exit
end

if ARGV.length == 0 then
  STDERR.puts o.help
  exit
end

pos = nil
start_dt = ""
r = ReadLog.new(ARGV[0])
w = WriteLog.new($save_fn)

ARGV.length.times do |i|
  r = ReadLog.new(ARGV[i])
  bucket = Array.new()
  w.write_line("*File Name   : #{ARGV[i]}")
  while l = r.read_line
    /\[([^\t]+)\]\t(?:[^\t]+\t)?([^\t]+)\/[^\t]+\t([^\t]+)\t(\d{4})\-(\d{2})\-(\d{2}) (\d{2}):(\d{2}):(\d{1}).+\t.+/ =~ l
    dt = $4 + $5 + $6 + $7 + $8 + $9
    bucket.length.times do |j|
      if bucket[j].bn == $2 then
        pos = j
      end
    end
    if !pos.nil? then
      bucket[pos].b_sum($1, $3, dt)
    else
      bucket << BucketLog.new($2, ARGV[i], dt)
      bucket[bucket.length - 1].b_sum($1, $3, dt)
    end
    pos = nil
  end
  r.close_file

  bucket.length.times do |j|
    w.write_line("-Bucket Name : #{bucket[j].bn}")
    w.write_line(" DateTime    : #{bucket[j].get_dt}")
    w.write_line(" HEAD        : #{bucket[j].head_cnt}")
    w.write_line(" PUT         : #{bucket[j].put_cnt}")
    w.write_line(" GET         : #{bucket[j].get_cnt}")
    w.write_line(" DELETE      : #{bucket[j].del_cnt}")
    w.write_line(" Capacities  : #{bucket[j].cap} Bytes")
    w.write_line(" Max         : #{bucket[j].get_max} qps")
    w.write_line(" Avarage     : #{bucket[j].get_avg} qph")
    w.write_line("")
  end
  bucket = nil
end
#.to_s.gsub(/(?<=\d)(?=(?:\d{3})+(?!\d))/, ',')
w.close_file
exit
