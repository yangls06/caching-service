require 'rubygems'
require 'net/http'
require 'rack'
require 'ruby-debug'

Debugger.start

class Application
   
  @@connection = Net::HTTP.new("0.0.0.0", 8000)

  def post_to_cacher(env, body, content_type, ids)
    post = Net::HTTP::Post.new(env["PATH_INFO"])
    post["X-Cache-Identifiers"] = ids.join(',')
    post["Content-Type"] = content_type
    #post["Host"] = nil
    post.body = body
    res = @@connection.request(post)
    puts "Cacher response: #{res.code}"
    puts res.body
  end

  def call(env)

    body = "The time is #{Time.now}\n\nenv = #{env.inspect}\n\n"
    content_type = "text/plain"

    #post_to_cacher(env, body, content_type, ["123"]) 

    [200, {"Content-Type" => content_type}, body]
  end
end

puts "Running at http://localhost:8001"
Rack::Handler::Mongrel.run(Application.new, :Port => 8001)
