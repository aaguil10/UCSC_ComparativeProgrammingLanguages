#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: bitstream.st,v 1.4 2012-03-07 14:23:13-08 - - $"
execname := '$RCSfile: bitstream.st,v $' =~ '.*: (.*),v.*' at: 1.

Object subclass: BitStream [
   |bitIndex byte myStream|
   nl := Character nl.
   count := 0.
   bitNumber := 8.
   bitBuffer := nil.
   outfile := nil.
   BIT0 := (0 bitAt: 0).
   BIT1 := (0 bitAt: 0).
   byteIndex := 0.
   myByte := 0.
   leaf := nil.
   leafToo := nil.
   tmptree := nil.
   tmpbit := nil.
   myStack := nil.
   i := 0.
   stop := false.
   t := nil.
   stop_read := false.
   tmp_stop := false.

   DEBUG_MODE := 0. "0 = none, 1 = tree, 2 = encoding, 3 = decoding"
   setd: x [DEBUG_MODE := x. ]

   BitStream class >> new: fileStream and: outStream [
      |result|
      result := super new.
      result init: fileStream and: outStream.
      ^result
   ]

   init: fileStream and: outStream[
      myStream := fileStream.
      outfile := outStream.
      bitIndex := 1.
      byteIndex := 1.
      self setbit1.
   ]

   setbit1 [
      myStream reset.
      self atEnd 
      ifTrue: [
         stop_read := true.
      ]
      ifFalse: [
         count := 0.
         [BIT1 = BIT0] whileTrue: [
            BIT1 := (self nextBit).
         ].
      ].
   ]

   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]

   atEnd [
      tmp_stop := bitIndex = 1 and: [myStream atEnd].
      tmp_stop ifTrue: [^true]
      ifFalse: [
         ^stop_read. 
         
      ].
      ^tmp_stop.
   ]

   stop_reading[
      "stdout << 'not reading no more' << Character nl."
      stop_read := true.
   ]


   build_huff_tree [
      self resetStream.
      stop_read ifTrue: [^nil] 
         ifFalse:[
         myStack := OrderedCollection new. 
         [stop] whileFalse: [
            tmpbit := (self nextBit).
            DEBUG_MODE = 3 ifTrue:[ stdout << '1: '<< tmpbit 
                                           << Character nl. ].
            tmpbit = BIT0 
               ifTrue: [
                  i := (self read1Byte).
                  DEBUG_MODE = 3 ifTrue:[ self printByte: i. ].
                  i = 0 ifTrue: [
                     tmpbit := (self nextBit).
                     tmpbit = 1 ifTrue: [stop := true.]
                     ifFalse: [
                        leaf := Leaf new: i count: 0.
                        myStack addFirst: leaf.
                     ].
                  ]
                  ifFalse:[
                     leaf := Leaf new: i count: 0.
                     myStack addFirst: leaf.
                  ]
               ]
               ifFalse: [
                  myStack size > 1
                  ifTrue: [
                     leafToo := myStack removeFirst.
                     leaf := myStack removeFirst.
                     tmptree := Tree new: leafToo char 
                        count: (leafToo count) 
                        + (leaf count) left: leaf right: leafToo.
                     myStack addFirst: tmptree.
                  ]
                  
            ].
         ].
         t := myStack removeFirst.
         DEBUG_MODE = 3 ifTrue:[ stdout << t << Character nl. ].
         
         [self atEnd] whileFalse: [
            t write_decompresed: self.    
         ].

         ^t.
      ]
   ]

   read1Byte [
      myByte := 0.
      myByte := myByte bitAt: 8 put: (self nextBit).
      myByte := myByte bitAt: 7 put: (self nextBit).
      myByte := myByte bitAt: 6 put: (self nextBit).
      myByte := myByte bitAt: 5 put: (self nextBit).
      myByte := myByte bitAt: 4 put: (self nextBit).
      myByte := myByte bitAt: 3 put: (self nextBit).
      myByte := myByte bitAt: 2 put: (self nextBit).
      myByte := myByte bitAt: 1 put: (self nextBit).
      ^myByte.
   ]

   writeByte: value [
      byteIndex := 9.
      count := 0.
      [count = 8] whileFalse: [
         self writeBit: (self nextB: value). 
         count := count + 1.
      ].
      DEBUG_MODE = 1 ifTrue:[ stdout << Character nl. ].
   ]

   nextB: value [
      byteIndex = 1 ifTrue: [
         byteIndex := 9.
      ].
      byteIndex := byteIndex - 1.
      ^(value asInteger) bitAt: byteIndex
   ]

   printByte: val [
      stdout << (val bitAt: 8).
      stdout << (val bitAt: 7).
      stdout << (val bitAt: 6).
      stdout << (val bitAt: 5).
      stdout << (val bitAt: 4).
      stdout << (val bitAt: 3).
      stdout << (val bitAt: 2).
      stdout << (val bitAt: 1).
      stdout << Character nl.
   ]

   clearByte [
      bitNumber := 8.
      bitBuffer := 0.
   ]

   flushByte [
      bitNumber = 8 ifFalse: [
         "stdout << Character nl << 'Flushing' << Character nl.
                  self printByte: bitBuffer."
         outfile nextPutByte: bitBuffer .
         self clearByte.
      ]
   ]

   flushRest [
         "stdout << Character nl << 'Flushing' << Character nl.
                  self printByte: bitBuffer."
      outfile nextPutByte: bitBuffer .
   ]


   writeBit: bit [
      bitBuffer isNil ifTrue: [self clearByte].
      bitNumber = 0 ifTrue: [self flushByte].
      DEBUG_MODE = 1 ifTrue:[ stdout << bit. ].
      DEBUG_MODE = 2 ifTrue:[ stdout << bit. ].
      DEBUG_MODE = 3 ifTrue:[ stdout << bit. ].
      bitBuffer := bitBuffer bitAt: bitNumber put: bit.
      bitNumber := bitNumber - 1.
   ]

   w1 [
      self writeBit: BIT1.
      DEBUG_MODE = 1 ifTrue:[ stdout << Character nl. ].
   ]

   w0 [
      self writeBit: BIT0.
      DEBUG_MODE = 1 ifTrue:[ stdout << Character nl. ].
   ]

   resetStream [
      |xe|
      myStream reset.
      self atEnd 
      ifTrue: [
         stop_read := true.
      ]
      ifFalse: [
         0 to: 3 do: [:a | 
            "self nextBit."
            DEBUG_MODE = 3 ifTrue: [stdout << (self nextBit).]
            ifFalse:[self nextBit.].
         ].
         DEBUG_MODE = 3 ifTrue: [stdout << Character nl.].
      ].
   ]

]
