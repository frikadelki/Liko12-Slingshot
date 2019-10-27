
-- Math Ext
--------------------------------------------------------------------------------------------------------------------------------

NumberMin = -1000000 --TODO
NumberMax =  1000000 --TODO

math.sign = function(n)
 if math.isZero(n) then
  return 0
 end
 if n > 0 then
  return 1
 else 
  return -1
 end
end

math.isZero = function(n, e)
 e = e or 0.99
 return 0 == math.floor(math.abs(n) + e)
end

math.pixel = function(n)
  return math.floor(n)
end

-- Table Ext
--------------------------------------------------------------------------------------------------------------------------------

table.findItem = function(items, item)
  for i, it in ipairs(items) do
    if (it == item) then
      return i
    end
  end
  return -1
end

table.removeItem = function(items, item)
  local index = table.findItem(items, item)
  if (index >= 0) then
    table.remove(items, index)
  end
end

-- Vec2
--------------------------------------------------------------------------------------------------------------------------------

Vec2 = {}

Vec2.new = function(x, y)
 return { x = x, y = y }
end

Vec2.vec = function(a, b)
 return { x = b.x - a.x, y = b.y - a.y }
end

Vec2.copy = function(v)
  return { x = v.x, y = v.y }
end

Vec2.set = function(a, b)
  a.x = b.x
  a.y = b.y
end

Vec2.sum = function(a, b)
 return { x = a.x + b.x, y = a.y + b.y }
end

Vec2.scaled = function(v, scale)
  if "number" ~= type(scale) then return error("Bad scale") end
  return { x = v.x*scale, y = v.y*scale }
end

Vec2.normalized = function(v)
  local len = Vec2.len(v)
  return { x = v.x/len, y = v.y/len }
end

Vec2.len2 = function(v)
  return v.x*v.x + v.y*v.y
end

Vec2.len = function(v)
  return math.sqrt(Vec2.len2(v))
end

Vec2.dot = function(a, b)
 return a.x*b.x + a.y*b.y
end

Vec2.normal = function(v, up)
  local len = Vec2.len(v)
  if up then
    return Vec2.new(-v.y/len,  v.x/len)
  else
    return Vec2.new( v.y/len, -v.x/len)
  end
end

Vec2.negated = function(v)
  return Vec2.new(-v.x, -v.y)
end

Vec2.rotated = function(v, alpha)
  if "number" ~= type(alpha) then return error("Bad alpha") end
  local sina = math.sin(alpha)
  local cosa = math.cos(alpha)
  local x = v.x*cosa - v.y*sina
  local y = v.x*sina + v.y*cosa
  return Vec2.new(x, y)
end

Vec2.isZero = function(v)
  return math.isZero(v.x) and math.isZero(v.y)
end

Vec2.fixZeroes = function(v)
 if math.isZero(v.x) then
  v.x = 0
 end
 if math.isZero(v.y) then
  v.y = 0
 end
end

function traversePolar(
  angleStart, angleEnd, angleMaxSteps, 
  rStart, rEnd, rSteps,
  visitor)
  local rLen = rEnd - rStart
  local angleLen = angleEnd - angleStart
  local rStep = rLen/rSteps
  for r = rStart, rEnd, rStep do
    local angleSteps = math.floor(angleMaxSteps*(r - rStart)/rLen)
    local angleStep = angleLen/(angleSteps ~= 0 and angleSteps or 1)
    for angle = angleStart, angleEnd, angleStep do
      visitor(angle, r)
    end
  end
end

-- Gamepad Utils
--------------------------------------------------------------------------------------------------------------------------------

local GamepadButtons = {
  Left = 1;
  Right = 2;
  Up = 3;
  Down = 4;
  A = 5;
  B = 6;
  Start = 7;
}

local Gamepad = {
  edges = { false, false, false, false, false, false, false };
}

function Gamepad:updateEdges(dt)
  for key, button in pairs(GamepadButtons) do
    local pressed = btn(button)
    if pressed then
      if self.edges[button] then
        self.edges[button] = self.edges[button] + dt
      else 
        self.edges[button] = 0
      end
    else
      self.edges[button] = false
    end
  end
end

function Gamepad:btn(button)
  return btn(button)
end

function Gamepad:btnEdge(button, dt)
  if not self.edges[button] then
    return false
  end
  return 0 == self.edges[button]
end

function Gamepad:btnLongPress(button, time)
  if not self.edges[button] then
    return false
  end
  local time = time or 0.5
  return self.edges[button] >= time
end

-- GPU Utils
--------------------------------------------------------------------------------------------------------------------------------

local Colors = {
  Black = 0;
  DeepBlue = 1;
  Magenta = 2;
  PastelGreen = 3;
  Brown = 4;
  Graphite = 5;
  Gray = 6;
  White = 7;
  Red = 8;
  Orange = 9;
  Yellow = 10;
  Green = 11;
  Blue = 12;
  PaleViolet = 13;
  Pink = 14;
  Skin = 15;
}

local TileSize = 8
local ScreenW, ScreenH = screenSize()
local ScreenCX, ScreenCY = ScreenW/2, ScreenH/2
local ScreenTilesW, ScreenTilesH = ScreenW/TileSize, ScreenH/TileSize

--------------------------------------------------------------------------------------------------------------------------------
-- Libraries -------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local class = Library("class")

--------------------------------------------------------------------------------------------------------------------------------
-- UI Widgets ------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local DebugWidgetsWireframe = false

WidgetGravity = {
  Center = {
    offsetSegment = function(base, segment)
      return (base - segment)/2
    end
  };
  Start = {
    offsetSegment = function(base, segment)
      return 0
    end
  };
  End = {
    offsetSegment = function(base, segment)
      return base - segment
    end
  };
}

-- Screen
--------------------------------------------------------------------------------------------------------------------------------

local ScreenPanel = class("ScreenPanel")

function ScreenPanel:initialize()
  self.widgets = {}
end

function ScreenPanel:addWidget(widget)
  if not widget then return error("Must provide widget.") end
  widget:setSize(ScreenW, ScreenH)
  table.insert(self.widgets, widget)
end

function ScreenPanel:draw()
  for i, widget in ipairs(self.widgets) do
    widget:draw()
  end  
end

-- Widget
--------------------------------------------------------------------------------------------------------------------------------

local Widget = class("Widget")

local WidgetSizeUnknown = -1

function Widget:initialize()
 self.width = WidgetSizeUnknown
 self.height = WidgetSizeUnknown
end

function Widget:setSize(width, height)
 self.width = width
 self.height = height
end

function Widget:draw()
 if self.width <= 0 then return error("Width not set.") end
 if self.height <= 0 then return error("Width not set.") end
 self:drawContent()
end

function Widget:drawContent() end

-- AbsBox
--------------------------------------------------------------------------------------------------------------------------------

local AbsBox = class("AbsBox", Widget)

function AbsBox:initialize(x, y, maxWidth, maxHeight)
  Widget:initialize(self)
  self.x = x or 0
  self.y = y or 0
  self.maxWidth = maxWidth or 0
  self.maxHeight = maxHeight or 0
  self.padding = { top = 0, left = 0, bottom = 0, right = 0 }
end

function AbsBox:setPosition(x, y)
  self.x = x or self.x
  self.y = y or self.y
end

function AbsBox:setMaxSize(width, height)
  self.maxWidth = width or self.maxWidth
  self.maxHeight = height or self.maxHeight
end

function AbsBox:setPadding(top, left, bottom, right)
  self.padding = { top = top, left = left, bottom = bottom, right = right }
end

function AbsBox:setWidget(widget)
  self.widget = widget
end

function AbsBox:drawContent()
  if not self.widget then return end
  if self.x >= self.width or self.y >= self.height then
    return
  end
  local boxX, boxY = self.x, self.y
  local boxWidth = math.min(self.width - boxX, self.maxWidth)
  local boxHeight = math.min(self.height - boxY, self.maxHeight)
  local contentX = boxX + self.padding.left
  local contentY = boxY + self.padding.top
  local contentWidth = boxWidth - self.padding.left - self.padding.right
  local contentHeight = boxHeight - self.padding.top - self.padding.bottom
  if contentWidth <= 0 or contentHeight <= 0 then
    return error("Not enought room for content.")
  end
  GPU.clip(boxX, boxY, boxWidth, boxHeight)
  GPU.pushMatrix()
  GPU.cam("translate", contentX, contentY)
  self.widget:setSize(contentWidth, contentHeight)
  self.widget:draw()
  GPU.popMatrix()
  if DebugWidgetsWireframe then
    GPU.rect(boxX, boxY, boxWidth, boxHeight, true, Colors.Red)
    GPU.rect(contentX, contentY, contentWidth, contentHeight, true, Colors.Red)
  end
  GPU.clip()
end

-- TextView
--------------------------------------------------------------------------------------------------------------------------------

local TextView = class("TextView", Widget)

function TextView:initialize(text)
 Widget:initialize(self)
 self.text = text or nil
 self.color = Colors.White
 self.gravity = WidgetGravity.Center
 self.linesGap = 1
end

function TextView:setText(text)
 self.text = text
end

function TextView:setColor(color)
 self.color = color
end

function TextView:setGravity(gravity)
 self.gravity = gravity
end

function TextView:setLinesGap(gap)
  self.linesGap = gap
end

function TextView:drawContent()
  if not self.text then return end
  local fontWidth, fontHeight = GPU.fontWidth(), GPU.fontHeight() - 1
  local maxWidth, wrappedText = GPU.wrapText(self.text, self.width)
  local minX = NumberMax
  local height = #wrappedText*(fontHeight + self.linesGap) - self.linesGap
  local y = self.gravity.offsetSegment(self.height, height)
  for i, line in ipairs(wrappedText) do
    local length = line:len()
    local width = length*(fontWidth + 1) - 1 -- the extra ones are auto-1-px-chars-spacing
    local x = self.gravity.offsetSegment(self.width, width)
    if (x < minX) then minX = x end
    GPU.color(self.color)
    print(line, x, y)
    y = y + fontHeight + self.linesGap
  end
  if DebugWidgetsWireframe then
    GPU.rect(minX, y - height - self.linesGap, maxWidth - 1, height, true, Colors.PaleViolet)
  end
end

--------------------------------------------------------------------------------------------------------------------------------
-- COLLIDER --------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

-- Collision Result
--------------------------------------------------------------------------------------------------------------------------------

local CollisionResult = class("CollisionResult")

function CollisionResult:initialize(point, axis)
  self.point = point
  self.axis = axis
end

-- Collision Body
--------------------------------------------------------------------------------------------------------------------------------

local CollisionBody = class("CollisionBody")

CollisionBodyType = {
  Polygon = {};
  Circle = {};
}

function CollisionBody:initialize()
  self.type = nil
end

function CollisionBody:setPolygon(shape)
  self.type = CollisionBodyType.Polygon
  self.shape = shape
end

function CollisionBody:setCircle(position, radius)
  self.type = CollisionBodyType.Circle
  self.position = position
  self.radius = radius
end

-- Collider
--------------------------------------------------------------------------------------------------------------------------------

local Collider = class("Collider")

function Collider:initialize()
  self.bodies = {}
end

function Collider:addBody(body)
  table.insert(self.bodies, body)
end

function Collider:checkBody(body)
  for i, other in ipairs(self.bodies) do
    if other ~= body then
      local collision = self:checkBodies(body, other)
      if collision then
        return collision
      end
    end
  end
  return nil
end

function Collider:checkBodies(bodyA, bodyB)
  -- TODO: we need to invert collision axis for one of symmetrical cases
  if (CollisionBodyType.Circle == bodyA.type) and (CollisionBodyType.Polygon == bodyB.type) then
    return self:checkPolygonCircle(bodyB, bodyA)
  elseif (CollisionBodyType.Polygon == bodyA.type) and (CollisionBodyType.Circle == bodyB.type) then
    return self:checkPolygonCircle(bodyA, bodyB)
  else
    return error("Not implemented body types collison check.")
  end
end

function Collider:checkPolygonCircle(polygon, circle)
  -- get separating axises
  -- each axis is `mark`-ed with touch-type collision point
  -- a point where bodies would have collided in a touching fashion
  local sases = {}

  local segmentsCount = #polygon.shape
  for isegment = 1, segmentsCount do
    local p1 = polygon.shape[isegment]
    local p2 = polygon.shape[isegment%segmentsCount + 1]

    -- each segment's normal is a separating axis
    local segment = Vec2.vec(p1, p2)
    local saSegment = Vec2.normal(segment)
    saSegment.mark = Vec2.sum(circle.position, Vec2.scaled(saSegment, circle.radius))
    table.insert(sases, saSegment)

    -- each vertex to circle center pair is a separating axis
    local toCircle = Vec2.vec(p1, circle.position)
    local saToCircle = Vec2.normalized(toCircle)
    saToCircle.mark = Vec2.copy(p1)
    table.insert(sases, saToCircle)
  end

  -- looking for overlaps along each separating axis
  local smallestOverlap = nil
  local smallestOverlapAxis = nil

  for isa, sa in ipairs(sases) do
    -- poly projection
    local polyMin, polyMax = NumberMax, NumberMin
    for ip, p in ipairs(polygon.shape) do
      local pp = Vec2.dot(sa, p)
      if pp < polyMin then polyMin = pp end
      if pp > polyMax then polyMax = pp end
    end
  
    -- circle projection
    local ccp = Vec2.dot(sa, circle.position)
    local circleMin = ccp - circle.radius
    local circleMax = ccp + circle.radius
  
    -- check overlap
    if polyMax < circleMin or polyMin > circleMax then
      return nil
    end

    -- store smallest overlap for collision info
    local overlap = nil
    if (polyMax < circleMax) then
      overlap = polyMax - circleMin
    else
      overlap = circleMax - polyMin
    end
    if not smallestOverlap or overlap < smallestOverlap then
      smallestOverlap = overlap
      smallestOverlapAxis = sa
    end
  end
  if not smallestOverlapAxis then return error("No overlap axis!") end
  return CollisionResult(smallestOverlapAxis.mark, smallestOverlapAxis)
end

--------------------------------------------------------------------------------------------------------------------------------
-- GAME ENTITIES ---------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local Entity = class("Entity")

function Entity:draw() end

function Entity:update(dt) end

--------------------------------------------------------------------------------------------------------------------------------

local Animation = class("Animation")

function Animation:initialize(time, loop)
  self.time = time or 1
  self.timer = self.time
  self.loop = loop or false
end

function Animation:restart()
  self.timer = 0
end

function Animation:stop()
  self.timer = self.time
end

function Animation:isFinished()
  return not self.loop and (self.timer >= self.time)
end

function Animation:update(dt)
  if self:isFinished() then
    return
  end
  self.timer = self.timer + dt
  if self:isFinished() then
    self.timer = self.time
    return
  end
  while self.timer > self.time do
    self.timer = self.timer - self.time
  end
end

function Animation:interpolateLinear(a, b)
  local ratio = self.timer/self.time
  return a + (b - a)*ratio
end

function Animation:interpolateTooth(offset, max)
  local timerRatio = self.timer/self.time
  if timerRatio < 0.5 then
    return offset + (max - offset)*timerRatio*2
  else
    return offset + (max - offset)*(1 - timerRatio)*2
  end
end

-- EXPLOSION
--------------------------------------------------------------------------------------------------------------------------------

local Explosion = class("Explosion", Entity)

function Explosion:initialize(position)
  self.position = position
  self.animation = Animation:new(0.5, false)
  self.animation:restart()
end

function Explosion:isFinished()
  return self.animation:isFinished()
end

function Explosion:update(dt)
  self.animation:update(dt)
end

function Explosion:draw()
  GPU.color(Colors.Orange)
  local radius = self.animation:interpolateLinear(1, 5)
  GPU.circle(self.position.x, self.position.y, radius)
end

-- PLANET
--------------------------------------------------------------------------------------------------------------------------------

local Planet = class("Planet", Entity)

local PlanetSurfaceNoise  = {
  pattern = { 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1 };

  sample = function(self, offset)
    return self.pattern[math.abs(offset)%#self.pattern + 1]
  end;
}

function Planet:initialize(x,y,r)
  Entity.initialize(self)
  self.position = Vec2.new(x, y)
  self.r = r

  self.mass = r*8

  self.body = CollisionBody:new()
  self.body:setCircle(self.position, self.r)

  self.renderSeed = 1
  self.renderMainColor = Colors.PastelGreen
  self.renderFeaturesColor = Colors.Green
end

function Planet:draw()
  GPU.pushMatrix()
  GPU.cam("translate", self.position.x, self.position.y)

  --main body
  GPU.color(self.renderMainColor)
  GPU.circle(0, 0, self.r)

  -- surface fuzz
  local surfaceHeight = 0.1
  traversePolar(
    0, 2.0*math.pi, 36,
    self.r + surfaceHeight - 3, self.r + surfaceHeight, 3,
    function(angleStep, radiusStep)
      local arm = Vec2.rotated(Vec2.new(0, radiusStep), angleStep)
      local step = math.floor(2.0*math.pi/angleStep)
      local noiseOffset = self.renderSeed + step
      local noise = PlanetSurfaceNoise:sample(noiseOffset)
      GPU.color(self.renderFeaturesColor)
      GPU.point(math.pixel(arm.x), math.pixel(arm.y))
    end)

  -- body features, sort of perlin
  local fpCellSize = math.floor(math.min(self.r, 8))
  local fpGridSize = math.floor(2*self.r/fpCellSize)
  local fpGridCoord = function(c)
    return math.abs(math.floor(c/fpCellSize))
  end

  local featurePerlin = function(angleStep, radiusStep)
    local arm = Vec2.rotated(Vec2.new(0, radiusStep), angleStep)
    local pxi, pxf = math.modf(arm.x + self.r + 1) -- we just move it all into positives
    local pyi, pyf = math.modf(arm.y + self.r + 1) -- we just move it all into positives
    local pxBox = fpGridCoord(pxi)
    local pyBox = fpGridCoord(pyi)
    if pxf < 0.33 then
      pxBox = pxBox - 1
    elseif pxf > 0.66 then
      pxBox = pxBox + 1
    end
    if pyf < 0.33 then
      pyBox = pyBox - 1
    elseif pyf > 0.66 then
      pyBox = pyBox + 1
    end
    local noiseOffset = self.renderSeed + pxBox + pyBox*fpGridSize
    local noise = PlanetSurfaceNoise:sample(noiseOffset)
    if 0 == noise%2 then
      GPU.color(self.renderFeaturesColor)
      GPU.point(math.pixel(arm.x), math.pixel(arm.y))
    end
  end

  traversePolar(
    0, 2*math.pi, 18,
    1, self.r - 1, math.floor(self.r/2),
    featurePerlin)
  
  traversePolar(
    0.35*math.pi, 2.35*math.pi, 18,
    1, self.r - 2, math.floor(self.r/4),
    featurePerlin)

  GPU.popMatrix()
end

-- SPACESHIP
--------------------------------------------------------------------------------------------------------------------------------

local Spaceship = class("Spaceship", Entity)

SpaceshipShape = {
  length = 12;
  forward = Vec2.new(1, 0);
  poly = {
    Vec2.new(-6, -3),
    Vec2.new(-6,  3),
    Vec2.new( 6,  0)
  };
}

SpaceshipControls = {
  Stasis = GamepadButtons.A;
  Burn = GamepadButtons.B;
  NoseLeft = GamepadButtons.Left;
  NoseRight = GamepadButtons.Right;
  BurnUp = GamepadButtons.Up;
  BurnDown = GamepadButtons.Down;

  DeltaNose = math.pi/12;
  DeltaBurn = 0.1;
}

function Spaceship:initialize(x, y)
  Entity.initialize(self)
  self.dead = false
  self.stasis = true

  self.position = Vec2.new(x, y)
  self.velocity = Vec2.new(0, 0)

  self.forward = SpaceshipShape.forward
  self.forwardAlpha = 0

  self.body = CollisionBody:new()
  self:updateBodyShape()

  self.stasisWait = false
  self.sasNoseToV = false
  
  self.burnPower = 0
  self.burnAnimation = Animation:new(0.5, false)
  self.burnDirection = Vec2.new(0, 0)
end

function Spaceship:explode(collision)
  if (self.dead) then return error("Already dead.") end
  self.dead = true
  self.stasis = true
  -- TODO: wierd
  return { Explosion:new(collision.point) }
end

function Spaceship:accelerate(a)
  if (self.stasis) then 
    return 
  end
  self:setSpeed(Vec2.sum(self.velocity, a))
end

function Spaceship:setSpeed(newv)
  Vec2.set(self.velocity, newv)
  Vec2.fixZeroes(self.velocity)
  self:alignNoseToV()
end

function Spaceship:alignNoseToV(v)
  v = v or self.velocity
  if Vec2.isZero(v) then 
    return 
  end
  self.forward = Vec2.normalized(v)
  self:updateForwardToAlpha()
end

function Spaceship:rotateNose(angle)
  self.forwardAlpha = self.forwardAlpha + angle
  self:updateForwardFromAlpha()
end

function Spaceship:updateForwardToAlpha()
  local alphaCos = Vec2.dot(self.forward, SpaceshipShape.forward)
  local alpha = math.acos(alphaCos)
  if not math.isZero(self.forward.y) then
    alpha = alpha*math.sign(self.forward.y)
  else
    if self.forward.x < 0 then
      alpha = math.pi
    else
      alpha = 0
    end
  end
  self.forwardAlpha = alpha
  self:updateBodyShape()
end

function Spaceship:updateForwardFromAlpha()
  self.forward = Vec2.rotated(SpaceshipShape.forward, self.forwardAlpha)
  self:updateBodyShape()
end

function Spaceship:updateBodyShape()
  local shape = {}
  for i, p in ipairs(SpaceshipShape.poly) do
    local rotated = Vec2.rotated(p, self.forwardAlpha)
    local translated = Vec2.sum(rotated, self.position)
    table.insert(shape, translated)
  end
  self.body:setPolygon(shape)
end

function Spaceship:checkAutoStasis(pullDirection)
  if not self.stasisWait then
    return
  end
  local dot = Vec2.dot(pullDirection, self.velocity)
  if math.isZero(dot) then
    self.stasis = true
    self.stasisWait = false
  end
end

function Spaceship:engageStasis()
  if self.stasis then
    return
  end
  if self.stasisWait then
    -- forced stasis at any point, think about that
    self.stasis = true
  else
    self.stasisWait = true
  end
end

function Spaceship:burn()
  if not self.burnAnimation:isFinished() then
    return 
  end
  local burnDirection = self.forward
  local burnPower = self.burnPower

  self.stasis = false
  self.stasisWait = false
  self.burnPower = 0

  local a = Vec2.scaled(burnDirection, burnPower)
  if not Vec2.isZero(a) then
    self:accelerate(a)
    self.burnDirection = burnDirection
    self.burnAnimation:restart()
  end
end

function Spaceship:checkInput()
  if self.dead then
    return
  end

  --stasis
  if Gamepad:btnEdge(SpaceshipControls.Stasis) then
    self:engageStasis()
  end

  -- nose direction
  if Gamepad:btnEdge(SpaceshipControls.NoseLeft) then
    self:rotateNose(-SpaceshipControls.DeltaNose)
  end
  if Gamepad:btnEdge(SpaceshipControls.NoseRight) then
    self:rotateNose(SpaceshipControls.DeltaNose)
  end

  --burn
  local checkDeltaBurn = function(button)
    return Gamepad:btnEdge(button) or Gamepad:btnLongPress(button)
  end

  if Gamepad:btn(SpaceshipControls.BurnUp) and Gamepad:btn(SpaceshipControls.BurnDown) then
    self.burnPower = 0
  else
    if checkDeltaBurn(SpaceshipControls.BurnUp) then
      self.burnPower = self.burnPower + SpaceshipControls.DeltaBurn
    end
    if checkDeltaBurn(SpaceshipControls.BurnDown) then
      self.burnPower = self.burnPower - SpaceshipControls.DeltaBurn
    end
  end
  
  if Gamepad:btnEdge(SpaceshipControls.Burn) then
    self:burn()
  end
end

function Spaceship:update(dt)
  self:checkInput()

  if not self.stasis then
    if not self.burnAnimation:isFinished() and 
       not Vec2.isZero(self.burnDirection) and
       not Vec2.isZero(self.velocity) then
      local velocityDirection = Vec2.normalized(self.velocity)
      local burnDirection = self.burnDirection
      local fullAlphaCos = Vec2.dot(velocityDirection, burnDirection)
      local fullAlpha = math.acos(fullAlphaCos)
      local noseAlpha = self.burnAnimation:interpolateLinear(0, fullAlpha)
      self:alignNoseToV(Vec2.rotated(burnDirection, -noseAlpha))
    end
    local dd = Vec2.scaled(self.velocity, dt)
    self.position = Vec2.sum(self.position, dd)
    self:updateBodyShape()
  end
  self.burnAnimation:update(dt)
end

function Spaceship:draw()
  local shape = self.body.shape

  -- body
  local coords = {}
  for i, p in ipairs(shape) do
    table.insert(coords, p.x)
    table.insert(coords, p.y)
  end
  GPU.color(Colors.Magenta)
  GPU.polygon(unpack(coords))

  -- burning torch
  if not self.burnAnimation:isFinished() then
    local torchSize = self.burnAnimation:interpolateTooth(1, 3)
    local backward = Vec2.negated(self.forward)
    local torchPosition = Vec2.sum(self.position, Vec2.scaled(backward, SpaceshipShape.length/2 + 2))
    GPU.color(Colors.Yellow)
    GPU.circle(torchPosition.x, torchPosition.y, torchSize)
  end

--[[-- Outline
  GPU.color(Colors.Graphite)
  for segment = 1, #shape do
    local p1 = shape[segment]
    local p2 = shape[segment%#shape + 1]
    GPU.line(p1.x, p1.y, p2.x, p2.y) --ship outline
  end
--]]
end

-- PLANE
--------------------------------------------------------------------------------------------------------------------------------

local Plane = class("Plane")

function Plane:initialize(width, height)
  self.width, self.height = width, height
  self.collider = Collider:new()
  self.planets = {}
  self.spaceships = {}
  self.explosions = {}
end

function Plane:addPlanet(planet)
  table.insert(self.planets, planet)
  self.collider:addBody(planet.body)
end

function Plane:addSpaceship(ship)
  table.insert(self.spaceships, ship)
  self.collider:addBody(ship.body)
end

function Plane:isAlive()
  for i, ship in ipairs(self.spaceships) do
    if not ship.dead then
      return true
    end
  end
  if 0 ~= #self.explosions then
    return true
  end
  return false
end

function Plane:update(dt)
  -- update ships
  for is, ship in ipairs(self.spaceships) do
    self:updateShip(ship, dt)
  end
  -- update explosions
  for ie, explosion in ipairs(self.explosions) do
    explosion:update(dt)
    if explosion:isFinished() then
      table.removeItem(self.explosions, explosion)
    end
  end
end

function Plane:updateShip(ship, dt)
  local scaledDt = (60*dt)
  local totalA = Vec2.new(0, 0)
  local maxA = nil
  -- gravity
  local applyForce = function(a)
    if (Vec2.len(a) < 0.01) then
      return
    end
    ship:accelerate(Vec2.scaled(a, scaledDt))
    
    totalA = Vec2.sum(totalA, a)
    if not maxA or Vec2.len2(a) > Vec2.len2(maxA) then
      maxA = a
    end
  end

  for ip, planet in ipairs(self.planets) do
    local a = self:planetToShipForce(ship, planet)
    applyForce(a)
  end

  -- auto stasis
  if maxA then
    ship:checkAutoStasis(maxA)
  end
  --ship:checkAutoStasis(totalA)

  -- udpate & collision
  ship:update(dt)
  self:checkShipCollisons(ship)
end

function Plane:planetToShipForce(ship, planet)
  local rline = Vec2.vec(ship.position, planet.position)
  local distanceSquared = Vec2.len2(rline)
  if distanceSquared < 1 then
    distanceSquared = 1
  end
  local distance = math.sqrt(distanceSquared)
  local power = planet.mass/distanceSquared
  local a = Vec2.scaled(rline, power/distance) --normalized rline
  return a
end

function Plane:checkShipCollisons(ship)
  if (ship.stasis) then 
    return 
  end
  local collision = self.collider:checkBody(ship.body)
  if collision then
    self:handleCollision(ship, collision)
   end
end

function Plane:handleCollision(ship, collision)
  if ship.stasis then return end
  local explosions = ship:explode(collision)
  for ie, explosion in ipairs(explosions) do
    table.insert(self.explosions, explosion)
  end
end

function Plane:draw()
  GPU.clear(Colors.DeepBlue)
  self:drawEntities(self.planets)
  self:drawEntities(self.spaceships)
  self:drawEntities(self.explosions)
end

function Plane:drawEntities(entities)
  for i, entity in ipairs(entities) do
    entity:draw()
  end   
end    

--------------------------------------------------------------------------------------------------------------------------------
-- STATE -----------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local GameState = class("GameState")

function GameState:update(dt) end

function GameState:draw() end

--------------------------------------------------------------------------------------------------------------------------------
-- GAME ------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------



-- HUD
--------------------------------------------------------------------------------------------------------------------------------

local GameHud = class("GameHud")

function GameHud:initialize()
  self.speedText = TextView:new()
  self.speedText:setGravity(WidgetGravity.Start)

  local speedBoxX = ScreenW/2 + 32
  local speedBoxHeight = 16
  self.speedBox = AbsBox:new(speedBoxX, ScreenH - speedBoxHeight, ScreenW - speedBoxX, speedBoxHeight)
  self.speedBox:setWidget(self.speedText)
  self.speedBox:setPadding(2, 2, 2, 2)

  self.controlsText = TextView:new()
  self.controlsText:setGravity(WidgetGravity.Start)

  local controlsBoxX = 0
  local controlsBoxHeight = 16
  self.controlsBox = AbsBox:new(controlsBoxX, ScreenH - controlsBoxHeight, ScreenW, controlsBoxHeight)
  self.controlsBox:setWidget(self.controlsText)
  self.controlsBox:setPadding(2, 2, 2, 2)

  self.panel = ScreenPanel:new()
  self.panel:addWidget(self.speedBox)
  self.panel:addWidget(self.controlsBox)
end

function GameHud:setSpeed(v)
  self.speedText:setText(
    string.format("VX: %+.3f", v.x).."\n"..
    string.format("VY: %+.3f", v.y))
end

function GameHud:setControls(burnPower, stasisMode)
  self.controlsText:setText(
    string.format("Burn dV: %+.2f", burnPower).."\n"..
    "Stasis: "..stasisMode..
    "")
end

function GameHud:draw()
  self.panel:draw()
end

-- STATE
--------------------------------------------------------------------------------------------------------------------------------

local GameStateGame = class("GameStateGame", GameState)

function GameStateGame:initialize()
  self.plane = Plane:new(ScreenW, ScreenH)
 
  local planetA = Planet:new(ScreenCX - 40, ScreenCY, 15)
  self.plane:addPlanet(planetA)

  local planetB = Planet:new(ScreenCX + 40, ScreenCY, 12)
  planetB.renderSeed = 2
  planetB.renderMainColor = Colors.Brown
  planetB.renderFeaturesColor = Colors.Graphite
  self.plane:addPlanet(planetB)
 
  self.spaceshipA = Spaceship:new(ScreenCX - 40, ScreenCY + 30, -16.3, 0)
  self.plane:addSpaceship(self.spaceshipA)

  self.hud = GameHud:new()
end

function GameStateGame:update(dt)
  -- update plane
  self.plane:update(dt)

  -- update hud
  local stasisMode = nil
  if self.spaceshipA.stasis then
    stasisMode = "hold"
  elseif self.spaceshipA.stasisWait then
    stasisMode = "auto"
  else
    stasisMode = "idle"
  end
  self.hud:setSpeed(self.spaceshipA.velocity)
  self.hud:setControls(self.spaceshipA.burnPower, stasisMode)
  
  -- check game over
  if not self.plane:isAlive() then
    gotoGameOver(self.plane)
  end
end

function GameStateGame:draw()
  self.plane:draw()
  self.hud:draw()
end

--------------------------------------------------------------------------------------------------------------------------------
-- MAIN MENU -------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local GameStateMainMenu = class("GameStateMainMenu", GameState)

function GameStateMainMenu:initialize()
  self.titleText = TextView:new(
    "".."\n"..
    "".."\n"..
    "S         S H O T".."\n"..
    "  L     G        ".."\n"..
    "    I N          ".."\n"..
    "".."\n"..
    "start game [c]".."\n"..
    "".."\n"..
    "".."\n"..
    "GAME CONTROLS".."\n"..
    "[left]/[right] - rotate ship".."\n"..
    "[up]/[down] - burn amount".."\n"..
    "[x] - burn".."\n"..
    "[z] - stasis")
  self.titleText:setLinesGap(3)
  self.titleText:setGravity(WidgetGravity.Center)

  self.titleBox = AbsBox:new(0, 0, ScreenW, ScreenH)
  self.titleBox:setWidget(self.titleText)
  self.titleBox:setPadding(16, 16, 16, 16)

  self.screen = ScreenPanel:new()
  self.screen:addWidget(self.titleBox)
end

function GameStateMainMenu:update(dt)
  if Gamepad:btnEdge(GamepadButtons.Start) then
    gotoNewGame()
  end
end

function GameStateMainMenu:draw()
  GPU.clear(Colors.Black)
  local drawOffsetX, drawOffsetY = 0, 0
  local tileX, tileY = 0, 0
  TileMap:draw(
    drawOffsetY, drawOffsetY,
    tileX, tileY,
    ScreenTilesW, ScreenTilesH)
  self.screen:draw()
end

--------------------------------------------------------------------------------------------------------------------------------
-- GAME OVER -------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local GameStateGameOver = class("GameStateGameOver", GameState)

function GameStateGameOver:initialize(plane)
  self.plane = plane

  self.gameOverText = TextView:new(
    "GAME OVER".."\n"..
    "".."\n"..
    "SCORE: ".."\n"..
    "".."\n"..
    "".."\n"..
    "press start (c)")
  self.gameOverText:setLinesGap(3)
  self.gameOverText:setGravity(WidgetGravity.Center)

  self.gameOverBox = AbsBox:new(0, 0, ScreenW, ScreenH)
  self.gameOverBox:setWidget(self.gameOverText)
  self.gameOverBox:setPadding(24, 24, 24, 24)

  self.screenPanel = ScreenPanel:new()
  self.screenPanel:addWidget(self.gameOverBox)
end

function GameStateGameOver:update(dt)
  self.plane:update(dt)
  if btn(GamepadButtons.Start) then
    gotoMainMenu()
  end
end

function GameStateGameOver:draw()
  self.plane:draw()
  self.screenPanel:draw()
end

--------------------------------------------------------------------------------------------------------------------------------
-- MAIN LOOP -------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

local gameState = GameStateMainMenu:new()

function gotoMainMenu()
  gameState = GameStateMainMenu:new()
end

function gotoNewGame()
  gameState = GameStateGame:new()
end

function gotoGameOver(game)
  gameState = GameStateGameOver:new(game)
end

--------------------------------------------------------------------------------------------------------------------------------

function _init()
end

function _update(dt)
  Gamepad:updateEdges(dt)
  gameState:update(dt)
end

function _draw(dt)
  gameState:draw()
end