require 'fileutils'

Dir.chdir("posts")
postFilePaths = Dir.glob("*")
postFilePaths.each do |postFilePath|
  puts("Editing #{postFilePath}")
  dir = File.dirname(postFilePath) + "/" + File.basename(postFilePath, '.*').gsub(/-ko$/,'')
  puts("Dir name: #{dir}")
  if !Dir.exist?(dir)
    puts("Creating #{dir}")
    Dir.mkdir(dir) 
  end
  originalPost = File.new(postFilePath)
  fileLines = originalPost.readlines
  lang = fileLines[5][-3,2]
  fileLines.delete_at(5)
  fileLines.delete_at(3)
  fileLines.delete_at(2)
  fileLines.delete_at(1)
  File.open("#{dir}/#{lang}.md", 'w') { |f| f.write(fileLines.join); f.close }
  originalPost.close
  FileUtils.rm(postFilePath)
end
