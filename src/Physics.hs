module Physics where

import Numeric

import Shapes
import Globals
import Rendering
import Linear.V2
import Linear.Metric
import ProjectMath


vectorFromAtoB :: (Float, Float) -> (Float, Float) -> (Float, Float)
vectorFromAtoB (x, y) (x', y') = (x'-x, y'-y)

vectorMagnitude :: (Float, Float) -> Float
vectorMagnitude (x, y) = (x**2 + y**2) ** (1/2)

-- splitVectorIntoComponents :: Float -> (Float, Float) -> (Float, Float)
-- splitVectorIntoComponents mag (nx, ny) = ( mag * cos angle , mag * sin angle )
--   where
--     angle = (pi/2) - atan2 nx ny

setAccelerationToZero :: Shape -> Shape
setAccelerationToZero shape = shape { sAcceleration = V2 0 0 }

applySpringForce :: V2 Float -> Shape -> V2 Float
applySpringForce sPos s = dxy * kSpring
  where
    pos = sPosition s
    toCenter = sPos - pos
    nCenter = normalize toCenter
    dxy = toCenter - scale 100 nCenter 

applyGravity :: Shape -> V2 Float
applyGravity s = gravity 

advanceTime :: Float -> Enviornment -> Enviornment 
advanceTime dt env = reactToWalls
  where
    applyForces = applyForceToShapes env
    advanceShapes = advanceAllShapes dt applyForces
    reactToWalls = reactToAllWalls dt bounceEff advanceShapes


applyForceToShapes :: Enviornment -> Enviornment 
applyForceToShapes env = env { eShapes = forcesApplied }
  where
    shapes = eShapes env
    resetAccelerations = map setAccelerationToZero shapes
    forcesApplied = map (applyForceToShape env) resetAccelerations

applyForceToShape :: Enviornment -> Shape -> Shape
applyForceToShape env shape = forcesApplied
  where
    forces = 
        [ const $ V2 0 0
        -- , applySpringForce (eCenter env)
        , applyGravity
        , applyDrag
        ]
        <*> [shape]
    totalForce = sum forces
    forcesApplied = 
        shape { sAcceleration  = sAcceleration shape + totalForce }

advanceAllShapes :: Float -> Enviornment -> Enviornment 
advanceAllShapes dt env = env { eShapes = advancedShapes}
  where
    shapes = eShapes env
    advancedShapes = map (advanceShape dt) shapes

advanceShape :: Float -> Shape -> Shape
advanceShape dt shape = shape 
    { sPosition = p2
    , sVelocity = v2
    , sLastPosition = p1 }
  where
    p1 = sPosition shape
    v1 = sVelocity shape
    a1 = sAcceleration shape
    v2 = v1 + scale dt a1
    p2 = p1 + scale dt v2

reactToAllWalls :: Float -> Float -> Enviornment -> Enviornment 
reactToAllWalls dt eff env = env { eShapes = bounceB} 
  where
    shapes = eShapes env
    h = fromIntegral screenHeight 
    w = fromIntegral screenWidth
    doesItHitL = doesCircleHitWall dt (V2 0 0) (V2 0 1)
    reactL = reactToSurface dt eff (V2 0 0) (V2 0 1)
    bounceL = fmapIf doesItHitL reactL shapes

    doesItHitT = doesCircleHitWall dt (V2 0 h) (V2 1 0)
    reactT = reactToSurface dt eff (V2 0 h) (V2 1 0)
    bounceT = fmapIf doesItHitT reactT bounceL

    doesItHitR = doesCircleHitWall dt (V2 w h) (V2 0 (-1))
    reactR = reactToSurface dt eff (V2 w h) (V2 0 (-1))
    bounceR = fmapIf doesItHitR reactR bounceT

    doesItHitB = doesCircleHitWall dt (V2 w 0) (V2 (-1) 0)
    reactB = reactToSurface dt eff (V2 w 0) (V2 (-1) 0)
    bounceB = fmapIf doesItHitB reactB bounceR

reactToAllObsticles :: Float -> Enviornment -> Enviornment 
reactToAllObsticles dt env = env

-- N from surf to point == -((P-A).D)D
-- https://stackoverflow.com/questions/5227373/minimal-perpendicular-vector-between-a-point-and-a-line
-- r=v−2(v⋅n)n
-- https://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector
reactToSurface :: Float -> Float -> V2 Float -> V2 Float -> Shape -> Shape
reactToSurface dt eff surfOrigin surfVector shape
    | norm (p-a) == 0 = error "Point on surface. Cannot resolve direction to bounce"
    | otherwise = shape  { sPosition = was
                         , sVelocity = reflectedVel }
  where
    (is, was) = isWas dt shape
    p = sPosition shape
    a = surfOrigin
    d = surfVector
    nHat = normalize $ p - a - scale ((p-a) `dot` d) (normalize d) 
    v = sVelocity shape
    reflectedVel = v - scale (2*eff*(v `dot` nHat)) nHat

-- | Looking left for ball, so the right wall should start at (w h) not (0 w)
doesCircleHitWall :: Float -> V2 Float -> V2 Float -> Shape -> Bool
doesCircleHitWall dt wallOrigin wallVector shape
    | isCircle shape = crossIs * crossWas < 0
    | otherwise = False
  where
    cPos = sPosition shape
    (Circle cRadius) = sGeometry shape
    (is, was) = isWas dt shape
    surfOrigin = 
        wallOrigin + scale cRadius (normalize (rotateV2 wallVector (-pi/2)))
    toIs = is - surfOrigin
    toWas = was - surfOrigin
    crossIs = signum $ crossZ wallVector toIs
    crossWas = signum $ crossZ wallVector toWas
    
isWas :: Float -> Shape -> (V2 Float , V2 Float)
isWas dt s = (sPosition s, sLastPosition s)
--   where
--     backStep = scale dt (sVelocity s)
--     was = sPosition s - backStep
    
applyDrag :: Shape -> V2 Float
applyDrag shape = scale (dragCoeff * norm v * a) (negate $ normalize v)
  where
    v = sVelocity shape
    a = case sGeometry shape of
        Circle r -> r
        Rectangle s1 s2 -> max s1 s2
        Triangle h b -> b
