#!/usr/bin/env ruby

require 'digest/md5'
require 'optparse'

class LogWriter
  def initialize
    if $save_fn == $stdout then
      @f = $save_fn
    else
      @f = open($save_fn, "w")
    end
  end

  def write_line(buff)
    @f.write(buff)
  end

  def close_file
    unless $save_fn == $stdout then
      unless @f.closed? then
        @f.close
      end
    end
  end
end

class FileReader
  def initialize(fn)
    prev_offset = 0
    count = 0
    err_count = 0
    err = 0

    op = LogWriter.new
    io = File.open(fn, "rb")

    op.write_line("File: #{fn}\n")

    # CHKSUM:128,KSIZE:16,BLEN_MSIZE:32,DSIZE:32,OFFSET:64,ADDRID:128,CLOCK:64,TIMESTAMP:42,DEL:1,BUF:437,CHUNK_SIZE:32,CHUNK_NUM:24,CHUNK_INDEX:24
    # KEY/binary,DATA/binary
    # PADDING:64
    io.read(194) # Skip Header

    while ! io.eof? do
      header     = io.read(128)
      md5        = header[  0, 16].unpack("H*")[0].hex.to_i  # CHKSUM:128
      ksize      = header[ 16,  2].unpack("H*")[0].hex.to_i # KSIZE:16
      dsize      = header[ 18,  4].unpack("H*")[0].hex.to_i # DSIZE:32
      #blen_msize = header[ 22, 4].unpack("H*")[0].hex.to_i  # BLEN_MSIZE:32
      offset     = header[ 26,  8].unpack("H*")[0].hex.to_i # OFFSET:64
      #addrid     = header[ 34, 16].unpack("H*")[0].hex.to_i # ADDRID:128
      #clock      = header[ 50,  8].unpack("H*")[0].hex.to_i # CLOCK:64
      timestamp  = header[ 58,  6].unpack("B42")[0]         # TIMESTAMP:42
      c_size     = header[118,  4].unpack("H*")[0].hex.to_i # CHUNK_SIZE:32
      c_num      = header[122,  3].unpack("H*")[0].hex.to_i # CHUNK_NUM:24
      del        = header[ 63    ].unpack("B3")[0]

      year   = timestamp[ 0, 12].to_i(2) # YEAR:12
      month  = timestamp[12,  6].to_i(2) # MONTH:6
      day    = timestamp[18,  6].to_i(2) # DAY:6
      hour   = timestamp[24,  6].to_i(2) # HOUR:6
      minute = timestamp[30,  6].to_i(2) # MINUTE:6
      second = timestamp[36,  6].to_i(2) # SECOND:6

      if del[2] == "1" then
        del_str = "*"
      else
        del_str = " "
      end

      key = io.read(ksize)
      if c_num >= 1 || md5 == 281949768489412648962353822266799178366 then
        dsize = 0
      end
      begin
        if dsize > 10485760 then
          raise "The dsize is too large"
        end
        body = io.read(dsize)
        ret_md5 = Digest::MD5.hexdigest(body).hex.to_i
  
        if (md5 == ret_md5 || md5 == 281949768489412648962353822266799178366) && (io.read(8) == "\0\0\0\0\0\0\0\0") then
          if err == 1 then
            op.write_line("[RECOVERD] offset: #{io.pos.to_s} count: #{count.to_s}\n")
          end
          err = 0
          prev_offset = io.pos
          if $key_list_flg then
            if key + "\n" != "\n" then
              op.write_line(sprintf("%04d-%02d-%02d %02d:%02d:%02d", year, month, day, hour, minute, second) + "\t#{del_str}\t#{prev_offset}\t#{dsize / 1024}KB")
              key_arr = key.split("\n")
              key_arr.length.times{|i|
                op.write_line("\t#{key_arr[i]}")
              }
              op.write_line("\n")
            else
              op.write_line("[ERROR] Key is not exists. #{offset}\n")
            end
          end
          count += 1
        else
          raise "The data block is broken"
        end
      rescue
        if err == 0 then
          p $!
          op.write_line("[ERROR] offset: #{prev_offset.to_s} count: #{count.to_s}\n")
          err_count += 1
        end
        if (prev_offset % 1000) == 0 then
            puts "Now skipping error blocks Offset: #{prev_offset.to_s}\n"
        end
        prev_offset += 1
        io.seek(prev_offset, IO::SEEK_SET)
        err = 1
      end
    end
    io.close
    op.close_file
    puts "Total: #{count.to_s} Offset: #{prev_offset.to_s} Error: #{err_count}\n"
  end
end

t = Time.now

$save_fn = $stdout
$key_list_flg = false
o = OptionParser.new
o.banner = "Usage : #{__FILE__} [avs_file] [-o output_file] [-l]"
o.on('-o output_file', '[output file name]', '(default=STDOUT)') {|v| $save_fn = v }
o.on('-l', '[key list]') {$key_list_flg = true}
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

print "Start: "
p t
ARGV.length.times do |i|
    FileReader.new(ARGV[i])
end
print "End  : "
p Time.now

exit 0
