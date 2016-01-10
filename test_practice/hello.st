#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

x := 5 2 raisedToInteger: 3.

addOne := [:i | i + 1].

b := [7].

stdout << (b ) << nl. 

Number extend [
   add: x [
      ^(self+x).
   ]
].

stdout << (9 add: 5) << nl.