#!/usr/bin/env ruby
#
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>
#


require "net/telnet"

puts Net::Telnet.new("Host" => "bofh.jeffballard.us", "Port" => 666).readlines.last[/^.+:(.*)/, 1].strip



=begin
Local Variables:
  mode:ruby
  fill-column:70
  indent-tabs-mode:nil
  ruby-indent-level:2
End:
=end
