These are a few haskell programs I wrote back in autumn quarter.

###apprpncalc.hs

This is a reverse polish notation calculator written to use the applicative structure in Haskell.

###compgeom.hs

Implementation of computational geometry in haskell using points, line segments, lines, parabolas, circles, triangles, and quadrilaterals.

An example of an output would be checking whether or not the following is True:

> intersects (Parabola 1 2 3) (LineSegment (Point 0 20) (Point 20 4))

###ExactCover.hs

After it is compiled, it accepts being passed in a file from stdin (e.g. ./ExactCover (<) input.txt - remove parentheses around (<)).

Sets are delimited by newline characters, and individual elements of each set are strings delimited by spaces.

The 'universe' is the set of all set elements (e.g. the universe of [1,2,3] and [4] is [1,2,3,4]).

A set of sets has 'exact cover' when there exists a mutually disjoint subset whose union contains all the elements in the 'universe.'

#tokenizercalc.hs

Written using monadic parsing of tokenized input.
