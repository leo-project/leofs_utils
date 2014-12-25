#!/usr/bin/env ruby

require 'optparse'

class ReadLog
  def initialize(fn)
    @f = open(fn)
    @fn = fn
    @buff = ''
    @method = ''
    @bucket = ''
    @path = ''
    @no = ''
    @cap = ''
    @date = ''
    @s_date = 0
    @s_code = ''
    @rows = 0
  end

  def read_line
    begin
      clear_value
      l = @f.gets
      @rows += 1
      parse_line_normal(l)
      if @method.nil? then
        parse_line_pre1_multi(l, @f.gets)
        @rows += 1
      end
      unless @method.nil? then
        @buff = "[#{@method}]\t#{@bucket}\t#{@path}\t#{@no}\t#{@cap}\t#{@date}\t#{@s_date}\t#{@s_code}\n"
        @s_date
      else
        @s_date = 0
      end
    rescue => e
      unless @f.closed?
        close_file
      end
      @buff = nil
    end
  end

  def get_line
    @buff
  end

  def get_date
    @s_date
  end

  def get_fn
    @fn
  end

  def get_rows
    @rows
  end

  def parse_line_normal(l)
    if /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t(.+)\t(\d{16})\t(\d+).*/ =~ l then
      @method = $1
      @bucket = $2
      @path   = $3
      @no     = $4
      @cap    = $5
      @date   = $6
      @s_date = $7.to_i
      @s_code = $8
    else
      /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t(\d{16})\t(\d+).*/ =~ l
      @method = $1
      @bucket = $2
      @path   = $3
      @no     = '0'
      @cap    = $4
      @date   = $5
      @s_date = $6.to_i
      @s_code = $7
    end
  end

  def parse_line_pre1_multi(l, m)
    l.gsub!("\n", "\t")
    l += m
    /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t(.+)\t(\d{16})\t(\d+).*/ =~ l
    @method = $1
    @bucket = $2
    @path   = $3
    @no     = $4
    @cap    = $5
    @date   = $6
    @s_date = $7.to_i
    @s_code = $8
  end

  def clear_value
    @method = ''
    @bucket = ''
    @path = ''
    @no = ''
    @cap = ''
    @date = ''
    @s_date = 0
    @s_code = ''
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

  def write_line(buff)
    @f.write(buff)
  end

  def close_file
    unless @f.closed? then
      @f.close
    end
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
  puts  "ERROR : unrecognized option"
  exit
end

if ARGV.length == 0 then
  STDERR.puts o.help
  exit
end

if $save_fn != $stdout && File.exists?($save_fn) then
  puts "[ERROR] Output file is already exists."
  exit
end

r = Array.new()
w = WriteLog.new($save_fn)
h = Hash.new()
t = Hash.new()
rows = 0
valid = 0

puts "Start: " + Time.now.strftime("%Y-%m-%d %H:%M:%S")

ARGV.length.times do |i|
  r[i] = ReadLog.new(ARGV[i])
  r[i].read_line
end

begin
  begin
    h.clear
    t.clear
    ARGV.length.times do |i|
      unless r[i].get_line.nil? then
        t[i] = r[i].get_date
        if t[i] == 0 then
          raise "Parse error. Filename: #{r[i].get_fn} Line: #{r[i].get_rows - 1}"
        end
      end
    end
    if t.length > 0 then
      h = t.sort_by{|f, d| d}
      if /^\[(PUT|DELETE)\].*/ =~ r[h[0][0]].get_line
        w.write_line(r[h[0][0]].get_line)
        valid += 1
      end
      r[h[0][0]].read_line
      rows += 1
      if rows % 1000 == 0 then
        print "."
      end
    end
  end while t.length > 0
  puts "\nTotal: #{rows} Valid: #{valid}"
  puts "End  : " + Time.now.strftime("%Y-%m-%d %H:%M:%S")
rescue => e
  puts "[ERROR] #{e.to_s}"
  exit
ensure
  w.close_file
  ARGV.length.times do |i|
    r[i].close_file
  end
end
