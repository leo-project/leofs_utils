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
    @f.write(l)
  end

  def close_file
    unless @f.closed?
      @f.close
    end
  end
end

class BucketLog
  attr_accessor :bn, :head_cnt, :put_cnt, :get_cnt, :del_cnt, :cap

  def initialize(bn)
    @bn = bn
    @head_cnt = 0
    @put_cnt = 0
    @get_cnt = 0
    @del_cnt = 0
    @cap = 0
    @max_cnt = 0
    @prev_dt = "0"
    @work_cnt = 0
  end

  def b_sum(l)
    /\[(.+)\]\t(.+)\t(.+)\t(\d{4})\-(\d{2})\-(\d{2}) (\d{2}):(\d{2}):(\d{1}).+\t(.+).+/ =~ l
    if $1 == "HEAD" then
      @head_cnt += 1
    elsif $1 == "PUT" then
      @put_cnt += 1
      @cap += $3.to_i
    elsif $1 == "GET" then
      @get_cnt += 1
    elsif $1 == "DELETE" then
      @del_cnt += 1
      @cap -= $3.to_i
    end

    now_dt = $4 + $5 + $6 + $7 + $8 + $9
    if @prev_dt == now_dt then
      @work_cnt += 1
    else
      @work_cnt = 1
      @prev_dt = now_dt
    end
    if @max_cnt < @work_cnt then
      @max_cnt = @work_cnt
    end
  end

  def get_max
    @max_cnt / 10.0
  end
end

$save_fn = $stdout
o = OptionParser.new
o.banner = "Usage : #{__FILE__} [log_file] [-o output_file]"
o.on('-o output_file', '[output file name]', '(default=STDOUT)') {|v| $save_fn = v }
o.on('-d seconds', '[second]', '(default=10)') {|v| $adj_sec = v }
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

bucket = Array.new()
pos = nil
start_dt = ""
r = ReadLog.new(ARGV[0])
w = WriteLog.new($save_fn)

while tmp = r.read_line
  /.+\t(.+)\/.+\t.+\t(\d{4})\-(\d{2})\-(\d{2}) (\d{2}):(\d{2}):(\d{2}).+\t.+/ =~ tmp
  dt = $2 + $3 + $4 + $5
  if start_dt == "" then
    start_dt = dt
  end

  #if start_dt == dt then
    bucket.length.times do |i|
      if bucket[i].bn == $1 then
        pos = i
      end
    end
    if !pos.nil? then
      bucket[pos].b_sum(tmp)
    else
      bucket << BucketLog.new($1)
      bucket[bucket.length - 1].b_sum(tmp)
    end
  #end
  pos = nil
end

bucket.length.times do |i|
  w.write_line("Bucket Name: #{bucket[i].bn}\n HEAD: #{bucket[i].head_cnt} PUT: #{bucket[i].put_cnt} GET: #{bucket[i].get_cnt} DELETE: #{bucket[i].del_cnt} Capacities: #{bucket[i].cap.to_s.gsub(/(?<=\d)(?=(?:\d{3})+(?!\d))/, ',') }Bytes Max: #{bucket[i].get_max}\n")
end
