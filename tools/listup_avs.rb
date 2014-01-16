#!/usr/bin/env ruby

require 'logger'
require 'optparse'
require 'digest/md5'

t = Time.now
fmt_t = Time.now.strftime("%Y%m%d%H%M%S")

$save_fn = $stdout
$log_fn = 'listup_avs_' + fmt_t + ".log"
$size = 5767168
o = OptionParser.new
o.banner = "Usage : #{__FILE__} [avs_file] [-s ] [-o output_file] [-l log_file]"
o.on('-o output_file', '[output file name]', '(default=STDOUT)') {|v| $save_fn = v }
o.on('-l log_file', '[log file name]', '(default=./listup_avs_YMDHMS.log)') {|v| $log_fn = v}
o.on('-s max_size', '[file max size]', '(default=5767168)') {|v| $size = v}
begin
  o.parse!
rescue
  STDERR.puts o.help
  puts "ERROR : unrecognized option"
  exit
end

if ARGV.length == 0 then
  STDERR.puts o.help
  exit 1
end

log = Logger.new($log_fn)

prev_offset = 0
count = 0
err = 0

if $save_fn == $stdout then
  op = $stdout
else
  op = File.open($save_fn, "w")
end
io = File.open(ARGV[0], "rb")

op.write("Start: " + t.strftime("%Y-%m-%d %H:%M:%S") + "\n")

# CHKSUM:128,KSIZE:16,BLEN_MSIZE:32,DSIZE:32,OFFSET:64,ADDRID:128,CLOCK:64,TIMESTAMP:42,DEL:1,BUF:437,CHUNK_SIZE:32,CHUNK_NUM:24,CHUNK_INDEX:24
# KEY/binary,DATA/binary
# PADDING:64
skip = io.read(194) # Skip Header

while ! io.eof? do
  header = io.read(128)
  md5 = header[0, 16].unpack("H*")[0] # CHKSUM:128
  ksize = header[16, 2].unpack("H*")[0].hex.to_i # KSIZE:16
  dsize = header[18, 4].unpack("H*")[0].hex.to_i # DSIZE:32
  #blen_msize = header[22, 4].unpack("H*")[0].hex.to_i # BLEN_MSIZE:32
  offset = header[26, 8].unpack("H*")[0].hex.to_i # OFFSET:64
  #addrid = header[34, 16].unpack("H*")[0].hex.to_i # ADDRID:128
  #clock = header[50, 8].unpack("H*")[0].hex.to_i # CLOCK:64
  timestamp = header[58, 6].unpack("B42")[0] # TIMESTAMP:42
  year = timestamp[0, 12].to_i(2) # YEAR:12
  month = timestamp[12,  6].to_i(2) # MONTH:6
  day = timestamp[18,  6].to_i(2) # DAY:6
  hour = timestamp[24,  6].to_i(2) # HOUR:6
  minute = timestamp[30,  6].to_i(2) # MINUTE:6
  second = timestamp[36,  6].to_i(2) # SECOND:6

  #del = (header[63].unpack("H*")[0].hex.to_i & 0x20) >> 5
  del = header[63].unpack("B3")[0] 
  if del[2] == "1" then
    del_str = "*"
  else
    del_str = " "
  end
  key = io.read(ksize)
  if dsize <= $size then
    body = io.read(dsize)
    ret_md5 = Digest::MD5.hexdigest(body)
  else
    body = ""
    ret_md5 = ""
    prev_offset -= 7
  end

  if (md5 == ret_md5) && (io.read(8) == "\0\0\0\0\0\0\0\0") then
    err = 0
    prev_offset = io.pos
    if key + "\n" != "\n" then
      op.write(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year, month, day, hour, minute, second))
      key_arr = key.split("\n")
      op.write("#{del_str} #{key_arr[0]}")
      for i in 1..key_arr.length - 1
        op.write(" #{key_arr[i]}")
      end
      op.write("\n")
    else
      log.error("key is not exists. #{offset}")
    end
    count += 1
    log.info("#{prev_offset} #{key}")
  else
    io.seek(prev_offset, IO::SEEK_SET)
    if err == 0 then
      log.error("Error File: #{key}")
    end
    while io.read(8) != "\0\0\0\0\0\0\0\0" do
      io.seek(prev_offset, IO::SEEK_SET)
      prev_offset += 1
    end
    prev_offset = io.pos
    if err == 0 then
      log.error("#{prev_offset} src_md5: #{md5} dst_md5: #{ret_md5}")
    end
    err = 1
  end
end

print "End  : "
p Time.now
puts "Total: " + count.to_s + " Offset: " + prev_offset.to_s

io.close
op.close

exit 0
