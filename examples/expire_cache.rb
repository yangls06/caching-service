#!/usr/bin/env ruby
require 'net/http'
require 'rubygems'
require 'ruby-debug'
Debugger.start

def post(ids)
  connection = Net::HTTP.new("0.0.0.0", 8000)

  p = Net::HTTP::Post.new("/_expire?identifiers=#{ids.join(",")}")
  res = connection.request(p)

  if res.code == "200"
    puts "deleted"
  else
    puts "error. #{res.code}"
    puts res.body
  end
end

post(ARGV)
