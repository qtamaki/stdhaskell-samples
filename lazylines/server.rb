require 'webrick'
require 'optparse'

def main
  addr = '127.0.0.1'
  port = 10081
  parser = OptionParser.new
  parser.banner = "Usage: #{File.basename($0)} [--bind=ADDR] [--port=NUM] [--debug]"
  parser.on('--bind ADDR', 'IP Address to bind (default: 127.0.0.1)') {|a|
    addr = a
  }
  parser.on('-p', '--port NUM', 'Port number to listen (default: 10081)') {|n|
    port = n.to_i
  }
  parser.on('--debug', 'Debug mode.') {
    $DEBUG = true
  }
  parser.on('--help', 'Prints this message and quit.') {
    puts parser.help
    exit 0
  }
  parser.on('--quit', 'Do nothing and quit (for mkexy command)') {
    exit 0
  }
  begin
    parser.parse!
  rescue OptionParser::ParseError => err
    $stderr.puts err.message
    $stderr.puts parser.help
    exit 1
  end

  $stderr.puts "*****************************************************"
  $stderr.puts "           Type Ctrl-C to stop server"
  $stderr.puts "*****************************************************"
  server = WEBrick::HTTPServer.new(
    :DocumentRoot => server_root(),
    :BindAddress  => addr,
    :Port         => port,
    :Logger       => new_logger(),
    :AccessLog    => access_log()
  )
  Signal.trap(:INT) { server.shutdown }
  server.start
end

module WEBrick
  class HTTPServer
    def mountfs(urlpath, path)
      mount(urlpath, WEBrick::HTTPServlet::FileHandler, path)
    end
  end
end

def new_logger
  WEBrick::Log.new($stderr, WEBrick::Log::DEBUG)
end

def access_log
  [ [ $stderr, WEBrick::AccessLog::COMMON_LOG_FORMAT  ] ]
end

def openlog(name)
  File.open("#{server_root()}/log/#{name}", 'a')
end

def server_root
  File.expand_path(File.dirname($0))
end

main
