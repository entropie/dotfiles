#!/usr/bin/ruby
#
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>
#


##
# $premshree$ $2005-03-30 19:31$
# Spell with Flickr
# Original idea: http://metaatem.net/words/
##

require 'flickr-ruby'
require 'net/http'

text = ARGV[0]
chars = text.split('')
imagemagickPath = "/usr/local/bin";

def dumpImage(url, filename)
	resp = Net::HTTP.get_response(URI.parse(url))
	imageData = resp.body
	File.open(filename, 'wb') { |f|
		f << imageData
	}
	filename
end

group = Group.new('27034531@N00')
execCmd = ''
chars.each { |char|
	photos =  group.getPhotos([char.upcase])
	photos << group.getPhotos([(char*2).upcase])
	photoId = photos[0]
	photoUrl = photoId.getURL('Thumbnail', 'source')
	p photoUrl
	dumpImage(photoUrl, char)
	execCmd += char + ' '
}
exec("#{imagemagickPath}/convert +append #{execCmd}#{text}")



=begin
Local Variables:
  mode:ruby
  fill-column:70
  indent-tabs-mode:nil
  ruby-indent-level:2
End:
=end
