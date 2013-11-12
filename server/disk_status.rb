#!/usr/bin/env ruby

require 'sinatra/base'

DEVICE = '/dev/sdb1'
DISK_USAGE_PATH = "#{DEVICE}/usage"

class DiskStatusApp < Sinatra::Base
  get DISK_USAGE_PATH do
    l = `df #{DEVICE}`
    
    /.+\n(.+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+%)\s+(.+)/ =~ l
    
    "available: #{$4}\ndevice: #{$1.strip}\ndisk: #{$6}\ntime: #{Time.now.to_i}\ntotal: #{$2}\nuse: #{$5}\nused: #{$3}"
  end
  
  # start the server if ruby file executed directly
  run! if app_file == $0
end


