#!/usr/bin/env ruby
#
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>
#

HTML = <<EOF
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
                      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<link rel="stylesheet" type="text/css" href="screen.css"/>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<style type="text/css">
body{margin-left:20%;margin-right:20%; font-size:1.3em}

</style>
<title></title>
</head>

<body>

%%%BODY%%%

<address>ackro.ath.cx</address>
<!-- Created: Sun Mar  9 23:35:54 CET 2008 -->
<!-- time stamp start -->

<!-- time stamp end -->
</body> </html>
EOF

require "redcloth"
require "pathname"
contents = ARGF.readlines.join
if contents
  file = Pathname.new(File.expand_path('~/public_html/textile.tmp.html'))
  str = RedCloth.new(contents).to_html
  file.open('w+'){ |f| f.puts(str=HTML.gsub(/%%%BODY%%%/, str))  }
  puts "http://www1.ackro.ath.cx/~#{ENV['USER']}/textile.tmp.html"
end


=begin
Local Variables:
  mode:ruby
  fill-column:70
  indent-tabs-mode:nil
  ruby-indent-level:2
End:
=end
