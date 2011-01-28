#!/usr/bin/env ruby

class Grid
  COMMON = DATA.read
  attr_reader :steps

  def initialize(steps)
    @steps = steps.to_f
  end

  def to_s
    @o = []
    container
    grid_common
    grid
    prefix
    suffix
    @o.join("\n")
  end

  def to_file(name)
    File.open(name, 'w+') do |css|
      css.puts "/*",
               "  960 Grid System ~ Core CSS.",
               "  Learn more ~ http://960.gs/",
               "  Licensed under GPL and MIT",
               "*/"
      css.puts self
    end
  end

  def container
    start(".container_#{steps.to_i}") do
      [ 'margin-left: auto',
        'margin-right: auto',
        'width: 100%' ]
    end
  end

  def grid_common
    margin = 100 / steps
    sel = (1..steps).map{|n| ".grid_#{n}" }.join(', ')
    start(sel) do
      [ 'display: inline',
        'float: left',
        # FIXME: do we need margin?
        # "margin-left: #{margin}%",
        # "margin-right: #{margin}%",
      ]
    end
  end

  def grid
    (1..steps).each do |n|
      width = n * (100/steps)
      start(".container_#{steps.to_i} .grid_#{n}") do
        "width: %.2f%%" % width
      end
    end
  end

  def prefix
    (1...steps).each do |n|
      width = n * (100/steps)
      start(".container_#{steps.to_i} .prefix_#{n}") do
        "padding-left: %.2f%%" % width
      end
    end
  end

  def suffix
    (1...steps).each do |n|
      width = n * (100/steps)
      start(".container_#{steps.to_i} .suffix_#{n}") do
        "padding-right: %.2f%%" % width
      end
    end
  end

  private

  def start(sel)
    @o << [
      "#{sel} {",
      [yield].flatten.map{|l| l << ";" },
      "}"
    ].join(' ')
  end
end

abort "#{$0} <filename> <grid_n>" if ARGV.size < 2

filename = ARGV.shift
File.open(filename, 'w+') do |io|
  ARGV.each do |steps|
    io.puts Grid.new(steps.to_i)
  end
  io.puts Grid::COMMON
end

__END__
/* ............. Grid >> Children (Alpha ~ First, Omega ~ Last) ............. *\
\* .......................................................................... */

.alpha { margin-left: 0px; }
.omega { margin-right: 0px; }

/* ......................... Clear Floated Elements ......................... *\
|*.......... http://www.positioniseverything.net/easyclearing.html ...........*|
\* .............. http://sonspring.com/journal/clearing-floats .............. */

html body * span.clear,
html body * div.clear,
html body * li.clear,
html body * dd.clear
{
  background: none;
  border: 0;
  clear: both;
  display: block;
  float: none;
  font-size: 0;
  list-style: none;
  margin: 0;
  padding: 0;
  overflow: hidden;
  visibility: hidden;
  width: 0;
  height: 0;
}

/* http://www.positioniseverything.net/easyclearing.html */

.clearfix:after {
  clear: both;
  content: '.';
  display: block;
  visibility: hidden;
  height: 0;
}

.clearfix {
  display: inline-block;
}

* html .clearfix {
  height: 1%;
}

.clearfix {
  display: block;
}



=begin
Local Variables:
  mode:ruby
  fill-column:70
  indent-tabs-mode:nil
  ruby-indent-level:2
End:
=end
