require 'net/http'
require 'rubygems'
require 'ruby-debug'
Debugger.start

def post(filename)
  connection = Net::HTTP.new("0.0.0.0", 8000)

  p = Net::HTTP::Post.new("/test")
  p["X-Cache-Identifiers"] = "123 321"
  p["Content-Type"] = if ext = File.extname(filename).sub(".","")
                        MIME_TYPES[ext]
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

MIME_TYPES = {
  "ai"    => "application/postscript",
  "asc"   => "text/plain",
  "avi"   => "video/x-msvideo",
  "bin"   => "application/octet-stream",
  "bmp"   => "image/bmp",
  "class" => "application/octet-stream",
  "cer"   => "application/pkix-cert",
  "crl"   => "application/pkix-crl",
  "crt"   => "application/x-x509-ca-cert",
 #"crl"   => "application/x-pkcs7-crl",
  "css"   => "text/css",
  "dms"   => "application/octet-stream",
  "doc"   => "application/msword",
  "dvi"   => "application/x-dvi",
  "eps"   => "application/postscript",
  "etx"   => "text/x-setext",
  "exe"   => "application/octet-stream",
  "gif"   => "image/gif",
  "htm"   => "text/html",
  "html"  => "text/html",
  "jpe"   => "image/jpeg",
  "jpeg"  => "image/jpeg",
  "jpg"   => "image/jpeg",
  "js"    => "text/javascript",
  "lha"   => "application/octet-stream",
  "lzh"   => "application/octet-stream",
  "mov"   => "video/quicktime",
  "mpe"   => "video/mpeg",
  "mpeg"  => "video/mpeg",
  "mpg"   => "video/mpeg",
  "pbm"   => "image/x-portable-bitmap",
  "pdf"   => "application/pdf",
  "pgm"   => "image/x-portable-graymap",
  "png"   => "image/png",
  "pnm"   => "image/x-portable-anymap",
  "ppm"   => "image/x-portable-pixmap",
  "ppt"   => "application/vnd.ms-powerpoint",
  "ps"    => "application/postscript",
  "qt"    => "video/quicktime",
  "ras"   => "image/x-cmu-raster",
  "rb"    => "text/plain",
  "rd"    => "text/plain",
  "rtf"   => "application/rtf",
  "sgm"   => "text/sgml",
  "sgml"  => "text/sgml",
  "tif"   => "image/tiff",
  "tiff"  => "image/tiff",
  "txt"   => "text/plain",
  "xbm"   => "image/x-xbitmap",
  "xls"   => "application/vnd.ms-excel",
  "xml"   => "text/xml",
  "xpm"   => "image/x-xpixmap",
  "xwd"   => "image/x-xwindowdump",
  "zip"   => "application/zip",
}


post(ARGV.first)
