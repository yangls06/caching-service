require 'net/http'
require 'rubygems'
require 'rack/file'

def post(host, port, path, headers, body)
  socket = TCPSocket.new(host, port)

  header_string = "POST #{path} HTTP/1.0\r\n"
  headers["Content-Length"] ||= body.length
  headers.each_pair do |k,v|
    header_string << "#{k}: #{v}\r\n"
  end

  request = "#{header_string}\r\n#{body}"
  written = 0
  while(written < request.length)
    written += socket.write(request)
  end

  # response
  response = socket.read(1024*4)
end

def post_cache(filename)
  headers = {}
  headers["X-Cache-Identifiers"] = "123 321"
  headers["Content-Type"] = if ext = File.extname(filename)
                              Rack::File::MIME_TYPES[ext.sub(".","")]
                            else 
                              "application/octet-stream"
                            end
  body = File.read(filename)
  puts post("0.0.0.0", 8000, "/test", headers, body)
end



post_cache(ARGV.first)
