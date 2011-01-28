#!/usr/bin/env ruby
#
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>
#

require 'open-uri'
require 'hpricot'
require 'pp'

url = ARGV.shift or raise ArgumentError, "need url"

fails = []

eles = Hpricot(open(url)).search('//a[@href]').select { |e|
  e['href'] =~ /^ed2k:\/\//
}.map{ |e|
  c = e['href'].to_s
}.uniq.compact.sort.each do |ele|
  system('~/bin/mldonkey-submit "%s" >/dev/null 2>/dev/null' % ele)
  unless $?.success?
    fails << ele
  else
    puts ele[0..50] + "  :-)"
  end
end

print "FAILED ", fails.size unless fails.empty?




=begin
Local Variables:
  mode:ruby
  fill-column:70
  indent-tabs-mode:nil
  ruby-indent-level:2
End:
=end
