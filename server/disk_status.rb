#!/usr/bin/env ruby
require 'bundler'
Bundler.require

http_port = 7500

configure do
  set :port, http_port
  set :session, true
  set :bind, '0.0.0.0'
end

get '/dev101/usage' do
  dev = "/dev/sdb1"
  l = `df #{dev}`

  /.+\n(.+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+%)\s+(.+)/ =~ l

  "available: #{$4}\ndevice: #{$1.strip}\ndisk: #{$6}\ntime: #{Time.now.to_i}\ntotal: #{$2}\nuse: #{$5}\nused: #{$3}"
end
