input = File.open("input") do |file|
  file.gets_to_end.split(",").map { |i| i.to_i }
end

def solve(input : Array(Int32), up_to : Int32)
  seen = {} of Int32 => Int32
  input[0..-2].each_with_index { |num, i| seen[num] = i }

  last = input.last
  start = input.size - 1
  stop = up_to - 1
  (start...stop).each do |i|
    answer = i - seen.fetch(last) { i }
    seen[last] = i
    last = answer
  end
  last
end

puts solve input, 2020
puts solve input, 30000000