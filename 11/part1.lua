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
      if neighbors >= 4 then
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
  local onLeft = index % self.width == 1
  local onRight = index % self.width == 0
  local onTop = index <= self.width
  local onBottom = index > #self.cells - self.width
  local res = {}
  if not onTop then
    if not onLeft then
      table.insert(res, self.cells[index - (self.width + 1)])
    end
    table.insert(res, self.cells[index - self.width])
    if not onRight then
      table.insert(res, self.cells[index - (self.width - 1)])
    end
  end
  if not onLeft then
    table.insert(res, self.cells[index - 1])
  end
  if not onRight then
    table.insert(res, self.cells[index + 1])
  end
  if not onBottom then
    if not onLeft then
      table.insert(res, self.cells[index + (self.width - 1)])
    end
    table.insert(res, self.cells[index + self.width])
    if not onRight then
      table.insert(res, self.cells[index + (self.width + 1)])
    end
  end
  return res
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
local count = 0
local done = false
repeat
  local nextState = state:step()
  count = count + 1
  done = state == nextState
  state = nextState
until done
print(state:count('#'))