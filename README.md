# Generating Cracks Using the Space Colonization Algorithm

It's not very good. It doesn't look very much like the results
in henrybraun.info/files/cracks.pdf. But my intent was really
just to write a program in Haskell and this does compile.

# Organization

## app/Main.hs

Grabs a random number, calls mkCrack', then draws it using the
Gloss library.

## src/


### Lib.hs

Exports way too many library functions to be exposed in Main,
but it's worth looking at for the basic API I designed and to
get a sense of what's in there.

### Vector

Implements a vector as in linear algebra, not as in a
dynamically sized array.

### Edge

Cracks are represented as a collection of edges which are then
interpreted as lines in app/Main.hs

### Hypercube

Used as a search area for KDTree. It is pointlessly general
since the program can only actually render 2D cracks.

### KDTree

A k-dimensional tree. Implements insertion, deletion, and
nearest neighbor search.

### SpaceColonization

Implements the actual space colonization algorithm:
    1. Grow a vein / cracks towards the direction of nearby
       auxins (random points).
    2. If a vein/crack gets very close to an auxin / weakness
       point, remove that auxin/point.
    3. Repeat.

In order to make the cracks look a tiny bit more crack-like, 
I also added the limitation that in order to branch out or
split, then the amount of pull (towards auxins / weak points)
has to be above some threshold and that the angle can't be
too acute (otherwise the material would just keep tearing
along the same line, then split into two seams).

## Tests

Just some unit tests.
