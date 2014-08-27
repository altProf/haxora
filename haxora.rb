input_haml = File.read('input.haml')
tags_soup = {}
soup = input_haml.split("\n").each.with_index { |e, i| tags_soup[i+1] = e.gsub(' ', '').gsub('%', '')}
puts 'done'
