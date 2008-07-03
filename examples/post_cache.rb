require 'net/http'
require 'rubygems'
require 'rack/file'

def post(filename)
  connection = Net::HTTP.new("0.0.0.0", 8000)

  p = Net::HTTP::Post.new("/test")
  p["X-Cache-Identifiers"] = "123 321"
  p["Content-Type"] = if ext = File.extname(filename).sub(".","")
                        Rack::File::MIME_TYPES[ext]
                      else 
                        "application/octet-stream"
                      end
  puts p["Content-Type"]
  p.body = File.read(filename)
  res = connection.request(p)

  if res.code == "200"
    puts "uploaded cache sucessfully"
  else
    puts "error. #{res.code}"
    puts res.body
  end
end



post(ARGV.first)
