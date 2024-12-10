
def parse_input(filename)
  antenna = {}

  rows = 0
  cols = nil
  
  File.readlines(filename).each_with_index do |line, i|
    row = line.strip
    
    rows += 1
    if cols == nil
      cols = row.length()
    end
    row.split('').each_with_index do |c, j|
      if c != '.'
        if not antenna.key?(c)
          antenna[c] = Array.new()
        end
        antenna[c].push([i, j])
      end
    end
  end
  return rows, cols, antenna
end

def dist2(a, b)
  return (a[0] - b[0])**2 + (a[1] - b[1])**2
end

def in_line(a, b, c)
  return (b[1] - a[1]) * (c[0] - b[0]) == (c[1] - b[1]) * (b[0] - a[0])
end

def count_antinodes(rows, cols, antenna)
  antinodes = 0
  for row in 0..(rows-1) do
    for col in 0..(cols-1) do
      p = [row, col]
      is_antinode = false
      antenna.each_pair { |freq, pos_list|
        pos_list.combination(2) { |a1, a2|
          if (dist2(p, a1) == 4 * dist2(p, a2) or 4 * dist2(p, a1) == dist2(p, a2)) and in_line(p, a1, a2)
            is_antinode = true
          end
        }
      }
      if is_antinode
        antinodes += 1
      end
    end
  end
  return antinodes
end

def count_antinode_harmonics(rows, cols, antenna)
  harmonics = 0
  for row in 0..(rows-1) do
    for col in 0..(cols-1) do
      p = [row, col]
      is_antinode_harmonic = false
      antenna.each_pair { |freq, pos_list|
        pos_list.combination(2) { |a1, a2|
          if in_line(p, a1, a2)
            is_antinode_harmonic = true
          end
        }
      }
      if is_antinode_harmonic
        harmonics += 1
      end
    end
  end
  return harmonics
end

rows, cols, antenna = parse_input("input.txt")
antinodes = count_antinodes(rows, cols, antenna)
harmonics = count_antinode_harmonics(rows, cols, antenna)

puts "antinodes = #{antinodes}, harmonics = #{harmonics}"
