#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: tree.st,v 1.10 2012-02-23 18:57:51-08 - - $"

nl := Character nl.

Object subclass: Leaf [
   |char count encoding|
   char [ ^ char ]
   count [ ^ count ]
   encoding [ ^ encoding ]
   eof := 128.

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   setEncoding: aBitString [
      encoding := aBitString.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char asCharacter 
                << ',' << count << ',' << encoding
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   rec: encoderString [
      encoding := encoderString.
   ]

   vist_all_leaves: storageContaner[
      storageContaner add: self.
   ]

   write_huffTree: bStream [
      bStream w0.
      bStream writeByte: char.
      "bStream writeByte: $b."
   ]

   write_encoded_char: x using: myStack [
      x = char 
         ifTrue: [^true]
         ifFalse: [^false].
         
   ]

   write_decompresed: bStream [
      "stdout << Character nl.
      stdout << char asCharacter << Character nl."
      char = eof 
         ifTrue: [
            bStream writeByte: char asCharacter. 
            bStream stop_reading.
         ]
         ifFalse:[ bStream writeByte: char asCharacter.].
   ]

]


Leaf subclass: Tree [
   |left right|
   sortedCharaters := nil.
   charInt := nil.
   tmpl := nil.
   tmpr := nil.
   tmp := nil.

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   rec: encoderString [
      left rec: encoderString, '0'.
      right rec: encoderString, '1'.
   ]

   vist_all_leaves: storageContaner[
      left vist_all_leaves: storageContaner.
      right vist_all_leaves: storageContaner.
   ]

   write_huffTree: bStream [
      left write_huffTree: bStream.
      right write_huffTree: bStream.
      bStream w1.
   ]

   write_encoded_char: x using: myStack [
      tmpl := (left write_encoded_char: x using: myStack) = true.
         tmpl ifTrue: [ myStack addFirst: 0. ^true]
         ifFalse: [ 
            tmpr := (right write_encoded_char: x using: myStack) = true.
            tmpr ifTrue: [ myStack addFirst: 1. ^true] 
               ifFalse: [^false].
         ]
   ]


   print_encoding_table: outfile[
      sortedCharaters := SortedCollection sortBlock: 
         [:a :c | a char > c char]. 
      self vist_all_leaves: sortedCharaters.
      sortedCharaters do: [:a |
         charInt := ((a char) asInteger).
         (charInt > 33) & (charInt < 127)
            ifTrue: [
               outfile << 
                  a char << '   ' <<
                  a count << '     ' <<
                  a encoding 
               << Character nl.
            ]
            ifFalse: [
               outfile << 
                  charInt << '   ' <<
                  a count << '     ' <<
                  a encoding 
               << Character nl.

               ].

      ].
   ]

   write_decompresed: bStream [
      (bStream atEnd) ifFalse: [
            tmp := (bStream nextBit).
            "stdout << tmp."
            tmp = (0 bitAt: 0)
               ifTrue: [ left write_decompresed: bStream]
               ifFalse: [ right write_decompresed: bStream].
      ]
   ]

]

Object subclass: HuffTree [
   |input_file|
   input_file [ ^ input_file ]
   frequency_table := nil.
   x := nil.
   myPQ := nil.
   count := 0.
   tmp_leaf := nil.
   l := nil.
   r := nil.
   t := nil.
   out_file := nil.
   in_file := nil. 
   myStack := nil.
   thebit := nil.
   tmpByte := nil.
   empty_flag := false.

   eof := 128.
   
   is_file_empty[^empty_flag.]

   DEBUG_MODE := 0. "0 = none, 1 = tree, 2 = encoding"
   setd: x [DEBUG_MODE := x.]

   HuffTree class >> new [
      self shouldNotImplement
   ]

   HuffTree class >> new: infile output_file: outfile opt: x 
      with: bStream[
      |result|
      result := super new.
      out_file := outfile.
      in_file := infile.
      (x = 'c') | (x = 't') ifTrue: [result compress.].
      x = 'u' ifTrue: [result decompress: bStream.].
      ^result.
   ]

   isAscii: val [
      ^( (val asInteger > 0) & (val asInteger < 257))
   ]

   compress [
      in_file reset.
      "Create Freaquency Table"
      frequency_table := Array new: 300.
      1 to: 300 do: [:a |  (frequency_table at: a put: 0) ].
      [(x := in_file next) notNil]  
      whileTrue: [
         (self isAscii: x)
            ifTrue:[ 
            "stdout << 'FT index: ' << x asInteger << Character nl."
            frequency_table at: (x asciiValue) 
               put: ((frequency_table at: (x asciiValue))+ 1).
            ]
            ifFalse:[
               "stderr << 'Not Ascii: ' << x asInteger << Character nl."
               frequency_table at: 257 
               put: ( (frequency_table at: 257 ) + 1).
            ]. 
      ].

      "Put leaves in Priority queue"
      myPQ := SortedCollection sortBlock: [:a :c | 
         a count = c count
            ifTrue:[a char > c char]
            ifFalse: [a count > c count]
      ].


      frequency_table do: [:a |
         count := count + 1.
         a > 0
            ifTrue: [
               (a asInteger) = 0
                  ifTrue:[
                     stderr << 'Added: ' << x asInteger << Character nl.
                     tmp_leaf := Leaf new: count count: a.
                     myPQ add: tmp_leaf.
                  ]
                  ifFalse:[
                     tmp_leaf := Leaf new: (count asCharacter) count: a.
                     myPQ add: tmp_leaf.
                  ] 
            ] 
      ].

      myPQ size = 0 ifTrue:[empty_flag := true.].

      "adding eof charater" 
      tmp_leaf := Leaf new: (eof asCharacter) count: 1.
      myPQ add: tmp_leaf.

      "Build Tree!"
      (myPQ size) > 1 ifFalse: [^nil].
      [(myPQ size) > 1] whileTrue: [
         l := myPQ removeLast.
         r := myPQ removeLast.
         t := Tree new: r char count: (r count) + (l count) 
            left: l right: r.
         myPQ add: t.
      ].
      t := myPQ removeLast.

      "Creates encoding string for each leaf."
      t rec: ''.

      stdout << t << Character nl.
      
      ^t.
   ]

   print_encoding_table [
      t print_encoding_table: out_file.
   ]

   write_huffTree: bStream [
      t write_huffTree: bStream.
      "Write EOF"
      1 to: 9 do: [:a| bStream w0].
      bStream w1.
   ]

   write_encoded_ver: bStream [
      in_file reset.
      [(x := in_file next) notNil]  whileTrue: [
         "stdout << Character nl."
         self write_encoded_char: x to: bStream.
      ].
      self write_encoded_char: eof asCharacter to: bStream.
      DEBUG_MODE = 2 ifTrue:[ stdout << Character nl. ]
   ]

   write_encoded_char: x to: bStream[
      myStack := OrderedCollection new. 
      t write_encoded_char: x using: myStack.
      myStack do: [:a |
         a = 1 
            ifTrue: [bStream w1. "stdout << '1'."]
            ifFalse: [bStream w0. "stdout << '0'."].
      ].
      DEBUG_MODE = 2 ifTrue:[ stdout << ' '. ]
   ]

   decompress: bStream [
      thebit = bStream build_huff_tree.
      
   ]
]



