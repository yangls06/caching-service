require 'rubygems'
require 'net/http'
require 'rack'
require 'ruby-debug'

Debugger.start

class Application
   
  @@connection = Net::HTTP.new("0.0.0.0", 8000)

  BOUNDARY = "AaB03x"
  def post_to_cacher(env, body, content_type, ids)
    message = ""
    message << "--#{BOUNDARY}\r\n"
    { "Content-Disposition" => 'form-data; name="cache"',
      "Content-Type" => content_type,
      "Content-Transfer-Encoding" =>  "binary"
    }.each_pair { |k,v| message << "#{k}: #{v}\r\n" }
    message << "\r\n#{body}"

    message << "\r\n--#{BOUNDARY}\r\n"
    { "Content-Disposition" => 'form-data; name="identifiers"',
    }.each_pair { |k,v| message << "#{k}: #{v}\r\n" }
    message << "\r\n#{ids.join(",")}"

    message << "\r\n--#{BOUNDARY}--\r\n"


    post = Net::HTTP::Post.new(env["PATH_INFO"])
    post["Content-Type"] = "multipart/form-data, boundary=#{BOUNDARY}"
    post.body = message
    res = @@connection.request(post)
    puts "Cacher response: #{req.code} \n #{req.body}"
  end

  def call(env)

    body = "The time is #{Time.now}\n\nenv = #{env.inspect}\n\n"
    content_type = "text/plain"

    post_to_cacher(env, body, content_type, ["123"]) 

    [200, {"Content-Type" => content_type}, body]
  end
end

puts "Running at http://localhost:8001"
Rack::Handler::Mongrel.run(Application.new, :Port => 8001)
