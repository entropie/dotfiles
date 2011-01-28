#
#
# Author:  Michael 'entropie' Trommer <mictro@gmail.com>
#

require "rubygems"
require "hpricot"
require "open-uri"

BaseUrl = "http://www.skinnermike.com/blog/page/%s/"

i = 0

while i+=1
  pageurl = BaseUrl % i
  doc = Hpricot(open(pageurl))

  entry = doc.search("div.entry")
  entry.each do |e|
    url = (e/"h3"/"a")
    if url.text =~ /skinimix/i
      p url
    end
  end
  
  exit if entry.size.zero?
end


=begin
Local Variables:
  mode:ruby
  fill-column:70
  indent-tabs-mode:nil
  ruby-indent-level:2
End:
=end
