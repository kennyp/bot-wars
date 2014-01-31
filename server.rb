require 'sinatra'
require 'open3'
require 'pry'


post '/' do
  Open3.popen2('Generic', 'rw') do |stdin,stdout|
    stdin.puts params[:data]
    stdout.read
  end
end
