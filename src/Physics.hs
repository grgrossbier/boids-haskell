module Physics where

import Data.List

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
    applySteering = applySteeringToBirds applyForces
    advanceShapes = advanceAll dt applySteering
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

advanceAll :: Float -> Enviornment -> Enviornment 
advanceAll dt env = env { eShapes = advancedShapes
                        , eBirds = advancedBirds}
  where
    shapes = eShapes env
    advancedShapes = map (advanceShape dt) shapes
    birds = eBirds env
    advancedBirds = map (advanceBird dt) birds

advanceShape :: Float -> Shape -> Shape
advanceShape dt shape = shape 
        { sPosition = p2
        , sVelocity = v2
        , sLastPosition = p1
        , sAngle = if isTriangle shape then unangle v2 else sAngle shape
        }
  where
    p1 = sPosition shape
    v1 = sVelocity shape
    a1 = sAcceleration shape
    v2 = v1 + scale dt a1
    p2 = p1 + scale dt v2

advanceBird :: Float -> Shape -> Shape
advanceBird dt shape = shape 
        { sPosition = p2T
        , sVelocity = v2T
        , sLastPosition = p1
        }
  where
    p1 = sPosition shape
    v1 = sVelocity shape
    ang = sAngle shape
    v2T = rotateV2 (V2 (norm v1) 0) ang
    p2T = p1 + scale dt v2T

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
    
applyDrag :: Shape -> V2 Float
applyDrag shape = scale (dragCoeff * norm v * a) (negate $ normalize v)
  where
    v = sVelocity shape
    a = case sGeometry shape of
        Circle r -> r
        Rectangle s1 s2 -> max s1 s2
        Triangle h b -> b

applySteeringToBirds :: Enviornment -> Enviornment 
applySteeringToBirds env = env { eBirds = appliedAvoidance }
  where
    birds = eBirds env
    kSep = eKSeparation env
    kAlign = eKAlignment env
    kCoh = eKCohesion env
    kAvoid = eKAvoidance env
    appliedSeperation = applyToEachInFlock False (1.5 * effectiveRadius simpleTriangle) (applySeperation kSep) birds
    appliedAlignment = applyToEachInFlock True (maxDist/2) (applyAlignment kAlign) appliedSeperation
    appliedCohesion = applyToEachInFlock True maxDist (applyCohesion kCoh) appliedAlignment
    appliedAvoidance = map (applyAvoidance 30 kAvoid env) appliedCohesion

applySeperation :: Float -> [Shape] -> Shape -> Shape
applySeperation k others target
    | null others = target
    | others == [target] = target
    | otherwise = target { sAngle = limitAngle newAngle } --limitAngle $ oldAngle + turn }
  where
    pushVectors = map (getPushVector target) others
    numOthers = fromIntegral $ length others
    compositeVector = sum pushVectors * (V2 (k/100) (k/100)) / numOthers
    oldAngleVector = angle $ sAngle target
    newAngle = unangle $ oldAngleVector + compositeVector
    -- compositeVector = sum pushVectors / numOthers
    -- oldAngle = sAngle target
    -- turn = 
    --     limitTurn 0.1 (-0.1) $ ((limitAngle . unangle) compositeVector - oldAngle) / (10*k)

applyCohesion :: Float -> [Shape] -> Shape -> Shape
applyCohesion k others target
    | null others = target
    | others == [target] = target
    | otherwise = target { sAngle = limitAngle newAngle }
  where
    pullVectors = map (`getPushVector` target) others
    numOthers = fromIntegral $ length others
    compositeVector = sum pullVectors * (V2 (k/100) (k/100))
    oldAngleVector = angle $ sAngle target
    newAngle = unangle $ oldAngleVector + compositeVector
    -- turn = simpleLimit $ 
    --     ((limitAngle . unangle) compositeVector - oldAngle) / (10*k)
    

applyAlignment :: Float -> [Shape] -> Shape -> Shape
applyAlignment k others target
    | null others = target
    | others == [target] = target
    | otherwise = target { sAngle = limitAngle $ oldAngle + turn }
  where
    numOthers = fromIntegral $ length others
    avgAlignment = (sum $ map (limitAngle . sAngle) others) / numOthers
    oldAngle = sAngle target
    turn = limitTurn 0.1 (-0.1) $ (k*avgAlignment - oldAngle) / (10*(k+1))

applyAvoidance :: Float -> Float -> Enviornment -> Shape -> Shape
applyAvoidance thresh k env target
    | null allObst = target
    | otherwise = target { sAngle = limitAngle newAngle }
  where
    allObst = eShapes env ++ eObsticles env
    pushVectors = 
        map (getPushVector target) 
        . filter (obsticleIsCloseEnough thresh target) 
        $ allObst
    compositeVector = sum pushVectors * (V2 (k/100) (k/100))
    oldAngleVector = angle $ sAngle target
    newAngle = unangle $ oldAngleVector + compositeVector
    -- compositeVector = sum pushVectors
    -- oldAngle = sAngle target
    -- turn = 
    --     simpleLimit ((limitAngle . unangle) compositeVector - oldAngle) / (10*k)

applyToEachInFlock :: Bool -> Float -> ([Shape] -> Shape -> Shape) -> [Shape] -> [Shape]
applyToEachInFlock includeSelf thresh f flock = appliedToAll
  where
    theseAreCloseEnough set bird = filter (isCloseEnough thresh bird) set
    self bird = if includeSelf then [] else [bird]
    applyToOneOfGroup set bird = f (theseAreCloseEnough set bird \\ self bird) bird
    appliedToAll = map (applyToOneOfGroup flock) flock

isCloseEnough :: Float -> Shape -> Shape -> Bool
isCloseEnough thresh bird1 bird2 = 
    distance (sPosition bird1) (sPosition bird2) < thresh

obsticleIsCloseEnough :: Float -> Shape -> Shape -> Bool
obsticleIsCloseEnough thresh bird ob = 
    distance (sPosition bird) (sPosition ob) < (thresh + effectiveRadius ob)


getPushVector :: Shape -> Shape -> V2 Float
getPushVector sTarget s = if sPosition sTarget == sPosition s
                            then V2 0 0
                            else normalize $ sPosition sTarget - sPosition s