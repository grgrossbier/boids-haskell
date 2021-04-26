# Boids

## Step 1: Learn basic animation. 
Moving Ball
 -- Applied forces (spring + gravity)

## Step 2: Learn interation with fixed enviornment
Bouncing Ball


## Step 3: Interacting bouncing balls
Add ball on click

## TODO:
 - Add Triangles
 - Three Rules:
    - separation: steer to avoid crowding local flockmates
    - alignment: steer towards the average heading of local flockmates
    - cohesion: steer to move towards the average position (center of mass) of local flockmates
 - Obsticle Avoidance

## Step 4:
 - Add Triangles 

## Step 5: 
 - Click for ball obsticle !
 - Click for triangle shape !
 - Move triangles (update angle according to velocity) !

## Step 6: 
 - Shapes are repelled from obsticles.
 - Something isn't working. 
 - Stopping Notes:
    X Scale down text.
    Switch shape to create a ball
    EXCEPT KSeperation - Include self in calculation
    Dial in K constants.

## Step 7:
 - Make Parallel
 - Benchmarking



Math Notes: 
- https://cs.stackexchange.com/questions/127295/algorithm-for-intersection-point-between-two-vectors
- intersectionOfLines
-     d1 = cross(p - a, b - a)
-     d2 = cross(q - a, b - a)
-     return (d1 * q - d2 * p) / (d1 - d2)

