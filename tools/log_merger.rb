#!/usr/bin/env ruby

require 'optparse'

class ReadLog
  def initialize(fn)
    @f = open(fn)
    @buff = ''
    @s_date = ''
  end

  def read_line
    begin
      l = @f.gets
      result = devide_line1(l)
      if result[0].nil? then
        result = devide_line2(l, @f.gets)
      end
      @buff = "[#{result[0]}]\t#{result[1]}\t#{result[2]}\t#{result[3]}\t#{result[4]}\t#{result[5]}\t#{result[6]}\t#{result[7]}\n"
      match_line(@buff)
    rescue
      unless @f.closed?
        @f.close
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

  def devide_line1(l)
    if /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t(.+)\t(\d{16})\t(\d+).*/ =~ l then
      return [$1, $2, $3, $4, $5, $6, $7, $8]
    else
      /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t(\d{16})\t(\d+).*/ =~ l
      return [$1, $2, $3, "0", $4, $5, $6, $7]
    end
  end

  def devide_line2(l, m)
    l.gsub!("\n", "\t")
    l += m
    /^\[(.+)\]\t(.+)\t(.+)\t(.+)\t(.+)\t(.+)\t(\d{16})\t(\d+).*/ =~ l
    return [$1, $2, $3, $4, $5, $6, $7, $8]
  end

  def match_line(l)
    if /^\[.+\]\t.+\t.+\t.+\t.+\t.+\t(\d{16})\t\d+.*/ =~ l then
      @s_date = $1.to_i
    else
      @s_date = nil
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
    unless @f.closed?
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

if File.exists?($save_fn) then
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
  h.clear
  t.clear
  ARGV.length.times do |i|
    unless r[i].get_line.nil? then
      t[i] = r[i].get_date
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

w.close_file
puts "\nTotal: #{rows} Valid: #{valid}"
puts "End  : " + Time.now.strftime("%Y-%m-%d %H:%M:%S")

exit
