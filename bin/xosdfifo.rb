#!/usr/bin/env ruby
#
# $Id: xosdfifo.rb 80 2007-02-07 19:27:59Z entropie $
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>

require "uri"

FIFO = File.expand_path('~/.osd')

`mkfifo "#{FIFO}"` unless test(?e, FIFO)
system('sudo chmod 0777 /dev/console')


require 'optparse'
require 'ostruct'
require 'xosd_scroll'

def parse(args)
  options = OpenStruct.new
  options.pos = XOSD::TOP
  options.offset = 50
  options.align = XOSD::LEFT
  options.indent = 30
  options.font = "-*-terminus-medium-*-*-*-32-*-*-*-*-*-*-15"
  options.color = "#F155D5"
  options.delay = 10
  options.lines = 10
  options.shadow = 3
  options.shadowcolour = "#000"
  options.outline = 1
  options.outlinecolour = "black"
  options.age = 0
  options.wait = 4
  options.displays = [ '0.0' ]
  
  opts = OptionParser.new do |opts|
    opts.banner = "Usage: #$0 [options]"
    opts.separator ""
    opts.separator "Specific options:"

    opts.on("-p", "--pos POS",
            "The vertical position of the text. POS can be top, middle, or bottom. The default is top.") do |p|
      case p
      when /top/i
        options.pos = XOSD::TOP
      when /middle/i
        options.pos = XOSD::MIDDLE
      when /bottom/i
        options.pos = XOSD::BOTTOM
      end
    end

    opts.on("-o", "--offset OFFSET") do |o|
      options.offset = o.to_i
    end

    opts.on("-A", "--align ALIGN") do |a|
      case a
      when /left/i
        options.align = XOSD::LEFT
      when /center/i
        options.align = XOSD::CENTER
      when /right/i
        options.align = XOSD::RIGHT
      end
    end

    opts.on("-i", "--indent OFFSET") do |o|
      options.indent = o.to_i
    end

    opts.on("-f", "--font FONT") do |f|
      options.font = f
    end

    opts.on("-c", "--colour COLOUR") do |o|
      options.color = o
    end

    opts.on("-D", "--displays DISPLAY") do |o|
      options.displays = o.split(',')
    end
    
    opts.on("-d", "--delay SECONDS") do |o|
      options.delay = o.to_i
    end

    opts.on("-l", "--lines LINES") do |o|
      options.lines = o.to_i
    end

    opts.on("-O", "--outline OFFSET") do |o|
      options.outline = o.to_i
    end

    opts.on("-u", "--outlinecolour OFFSET") do |o|
      options.outline_color = o
    end

  end
  opts.parse!(args)
  options
end

options = parse(ARGV)

osds = options.displays.inject([]) do |m, osd|
  begin
    ENV['DISPLAY'] = ":%s"%osd
    osd = XOSD::XosdScroll.new(options.lines)
    osd.position = options.pos
    osd.vertical_offset = options.offset
    osd.align = options.align
    osd.horizontal_offset = options.indent
    osd.font = options.font
    osd.color = options.color
    osd.shadow_offset = options.shadow
    osd.shadow_color = options.shadowcolour
    osd.outline_offset = options.outline
    osd.outline_color = options.outlinecolour
    osd.timeout = options.delay
    m << osd
  rescue
    p $!
    m << nil
  end
end

osds.compact!


def listen_fifo(special_file)
  File.open(special_file, File::NONBLOCK | File::RDONLY) do |fifo|
    loop do
      while !fifo.eof
        yield fifo.gets("\n").strip
      end
      sleep 2
    end
  end
end

threads = []

threads << Thread.new do
  listen_fifo(FIFO) do |fifo|
    osds.each do |osd|
      osd << fifo
    end
  end
end


threads.each{ |thread| thread.join } while true

