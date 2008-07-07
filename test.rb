require 'net/http'
require 'test/unit'

HOST = "0.0.0.0"
PORT = "8000"

def parse_http_response(response)
  response.sub!(%r{HTTP/(\d.\d) (\d\d\d) .*}, "")
  version = $1
  code = $2.to_i
  header_string, body = response.split("\r\n\r\n")
  headers = {}
  header_string.each_line do |line|
    field, value = line.split ':', 2
    next unless value
    headers[field] = value.chomp
  end
  headers["HTTP_VERSION"] = version

  [code, headers, body]
end

def build_request(method, path, headers, body="")
  header_string = "#{method} #{path} HTTP/1.0\r\n"
  headers["Content-Length"] ||= body.length
  headers.each_pair do |k,v|
    header_string << "#{k}: #{v}\r\n"
  end
  request = "#{header_string}\r\n#{body}"
end

def post(path, headers={}, body="")
  socket = TCPSocket.new(HOST, PORT)
  request = build_request("POST", path, headers, body)

  written = 0
  while(written < request.length)
    written += socket.write(request)
  end

  response = socket.read(1024*4)
  parse_http_response(response)
end

def get(path, headers={})
  socket = TCPSocket.new(HOST, PORT)
  request = build_request("GET", path, headers)

  written = 0
  while(written < request.length)
    written += socket.write(request)
  end

  response = socket.read(1024*4)
  parse_http_response(response)
end



class TestSimple < Test::Unit::TestCase
  def setup
    n = File.dirname(__FILE__) + "/start.sh"
    @t = Thread.new { %x{sh #{n}} }
    sleep  1
  end

  def teardown
    @t.kill
    %x{killall beam}
  end

  def test_get_empty_db
    gstatus, gheaders, gbody = get("/test")
    assert_equal 404, gstatus
  end

  def test_simple_post
    headers =  { "Content-Type" => "text/plain", "X-Cache-Identifiers" => "123 321" }
    pstatus, pheaders, pbody = post("/test", headers, "Hello World")
    assert_equal 200, pstatus

    gstatus, gheaders, gbody = get("/test")
    assert_equal 200, gstatus
    assert_equal "Hello World", gbody
  end

  def test_double_insert
    headers =  { "Content-Type" => "text/plain", "X-Cache-Identifiers" => "123 321" }
    pstatus, _, _ = post("/test", headers, "Hello World")
    assert_equal 200, pstatus

    headers =  { "Content-Type" => "text/plain", "X-Cache-Identifiers" => "123 321" }
    pstatus, _, _ = post("/test", headers, "Hello World")
    assert_equal 200, pstatus

    gstatus, gheaders, gbody = get("/test")
    assert_equal 200, gstatus
    assert_equal "Hello World", gbody
  end

  def test_expire
    path = "/something/blah"

    headers =  { "Content-Type" => "text/plain", "X-Cache-Identifiers" => "123 321" }
    pstatus, _, _ = post(path, headers, "Hello World")
    assert_equal 200, pstatus

    gstatus, _, gbody = get(path)
    assert_equal 200, gstatus
    assert_equal "Hello World", gbody

    pstatus, _, _ = post("/_expire?identifiers=123") 
    assert_equal 200, gstatus

    gstatus, _, _ = get(path)
    assert_equal 404, gstatus

    # try to repost now
    pstatus, _, _ = post(path, headers, "Hello World")
    assert_equal 200, pstatus

    gstatus, _, gbody = get(path)
    assert_equal 200, gstatus
    assert_equal "Hello World", gbody
  end

end
