#!/usr/bin/env ruby

#
# $Id$
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>

require "profiler"
require 'net/http'
require 'uri'

LANG = %w{C89 C C++ Java Pascal Perl PHP PL/I Python Ruby Scheme SQL VB XML Plain Text}
TABS = %w{No 2 3 4 5 6 8}
RAFB, RURL = %w{http://rafb.net/paste/paste.php
                 http://rubyurl.com/rubyurl/create}.map { |u| URI.parse(u) }

desc = 'Plain'
if ARGV.size == 0
  rurlmode = 'Ruby'
  desc = ''
elsif ARGV.size == 1
  rurlmode = ARGV.shift
  raise "lang not supported #{rurlmode}" unless LANG.include?(rurlmode)
  desc = ''
elsif ARGV.size == 2
  rurlmode = ARGV[0]
  desc = ARGV[1]
end

(form = {
   'lang' => (rurlmode || 'plain'),
   'nick' => ENV['USER'] || 'unknown',
   'desc' => desc,
   'cvt_tabs' => 'No',
   'text' => nil}).keys.each do |key|
  ARGV.reject! { |a| form[key] = $1 if a =~ /-#{key[0,1]}=?(.*)/ }
end


form['lang'] = LANG.detect {|l| l.casecmp(form['lang']) == 0} or
  raise "Unrecognized language (allowed values are #{LANG.inspect})" 
form['cvt_tabs'] = TABS.detect {|l| l.casecmp(form['cvt_tabs']) == 0} or
  raise "Unrecognized tab length (allowed values are #{TABS.inspect})" 
form['text'] ||= ARGF.read

begin
  res = (((q = [[RAFB, form]]).map do |url,form|
            if (res = Net::HTTP.post_form(url,form)).is_a?(Net::HTTPRedirection)
              if (loc = res['location']) =~ /\/rubyurl\/show\/(.*)/
                #"http://rubyurl.com/#$1"
                ''
              else
                rfb = loc
                #rfb = "http://rafb.net#{loc}"
                #rfb = loc
                #q << [RURL, {'rubyurl[website_url]' => rfb}] if rurlmode
                rfb
              end
            else
              #res.error!
            end 
          end)[(0)..-1].join("\n"))

  puts res
  # File.open('/home/mit/.osd', 'w') do |lp|
  #   lp.puts res
  # end

  # File.open('/home/mit/.lastpste', 'w+') do |lp|
  #   lp.puts res.strip
  # end
# rescue
#   p $!
#   p caller
#   $!.set_backtrace([]) and raise
  
end


# >> 
