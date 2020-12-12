Grid = {}
function Grid:new(data)
  local o = {cells = data.cells, width = data.width}
  setmetatable(o, self)
  self.__index = self
  return o
end
function Grid.__eq(a, b)
  for i, cell in ipairs(a.cells) do
    if b.cells[i] ~= cell then return false end
  end
  return true
end
function Grid:count(target)
  local count = 0
  for i, cell in ipairs(self.cells) do
    if cell == target then count = count + 1 end
  end
  return count
end
function Grid:print()
  local height = #self.cells / self.width
  for y=1,height do
    local start = (y - 1) * self.width + 1
    local stop = y * self.width
    local str = ''
    for x=start,stop do
      str = str .. self.cells[x]
    end
    print(str)
  end
  print()
end
function Grid:getCoords(index)
  local x = index % self.width
  local y = math.ceil(index / self.width)
  if x == 0 then x = self.width end
  return x, y
end
function Grid:getIndex(x, y)
  return ((y - 1) * self.width) + x
end
function Grid:step()
  local cells = {}
  for i, cell in ipairs(self.cells) do
    if cell == '.' then
      cells[i] = '.'
    elseif cell == 'L' then
      local neighbors = self:occupiedNeighbors(i)
      if neighbors == 0 then
        cells[i] = '#'
      else
        cells[i] = 'L'
      end
    else -- cell is '#'
      local neighbors = self:occupiedNeighbors(i)
      if neighbors >= 5 then
        cells[i] = 'L'
      else
        cells[i] = '#'
      end
    end
  end
  return Grid:new{cells=cells, width=self.width}
end
function Grid:occupiedNeighbors(index)
  local count = 0
  for _, cell in ipairs(self:neighbors(index)) do
    if cell == '#' then count = count + 1 end
  end
  return count
end
function Grid:neighbors(index)
  local res = {}
  res[#res+1] = self:neighborInDirection(-1, -1, index)
  res[#res+1] = self:neighborInDirection(0, -1, index)
  res[#res+1] = self:neighborInDirection(1, -1, index)
  res[#res+1] = self:neighborInDirection(-1, 0, index)
  res[#res+1] = self:neighborInDirection(1, 0, index)
  res[#res+1] = self:neighborInDirection(-1, 1, index)
  res[#res+1] = self:neighborInDirection(0, 1, index)
  res[#res+1] = self:neighborInDirection(1, 1, index)
  return res
end
function Grid:neighborInDirection(hspeed, vspeed, start)
  local x, y = self:getCoords(start)
  local height = #self.cells / self.width
  x = x + hspeed
  y = y + vspeed
  while x >= 1 and y >= 1 and x <= self.width and y <= height do
    local cell = self.cells[self:getIndex(x, y)]
    if cell ~= '.' then return cell end
    x = x + hspeed
    y = y + vspeed
  end
  return nil
end

function getInitialState()
  local cells = {}
  local width = 0
  for line in io.lines() do
    if width == 0 then width = #line end
    for x = 1, #line - 1 do
      local char = line:sub(x,x)
      cells[#cells+1] = char
    end
  end
  return Grid:new{cells=cells, width=width - 1}
end

local state = getInitialState()
local done = false
repeat
  local nextState = state:step()
  done = state == nextState
  state = nextState
until done
print(state:count('#'))
