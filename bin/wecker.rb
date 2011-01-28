#!/usr/bin/env ruby
#
# $Id: wecker.rb 79 2007-02-07 12:05:45Z entropie $
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>

# wecker.rb ensures that the alsasound is turned up
require "readline"
class Wecker
  attr_accessor :time_str

  TCARD = 1
  VOL = 95
  TARGETS = [ :PCM, :Master ]
  
  def initialize(time_str)
    @time_str = time_str
    volume?.each_pair do |dev, vol|
      if vol <= VOL
        volume!(TCARD, dev, VOL)
      end
    end
    play
  end

  def play
    str = "/usr/bin/alsaplayer '#{ARGV.join}'"
    system(str)
  end
  
  def volume!(card = TCARD, targets = TARGETS, vol = VOL)
    targets = [targets] unless targets.class == Array
    targets.each do |t|
      `amixer -c #{card} sset #{t},0 #{vol}`
    end
    volume?
  end
  
  def volume?(card = TCARD, targets = TARGETS)
    rets = Hash.new
    targets.each do |t|
      `amixer -c #{card} sget #{t},0`.to_a[-2..-1].map {|c|
        rets[t] = c.scan(/\[([0-9]+)%\]/).flatten.to_s.to_i
      }
    end
    rets
  end
  
end



Wecker.new(ARGV[0]).volume!















