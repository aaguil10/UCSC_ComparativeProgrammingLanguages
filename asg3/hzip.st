#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

FileStream fileIn: 'tree.st'.
FileStream fileIn: 'BitStream.st'.

DEBUG_FLAG := false.
TREE_FLAG := false.
COMPRESS_FLAG := false.
DECOMPRESS_FLAG := false.

in_file := nil.
out_file := nil.
nl := Character nl.


Smalltalk arguments size < 1
  ifTrue: [stderr << 'Please specify one argument and input file.'
    << ' If output file is not specified output will be printed to 
        stdout.' << nl.
  ]
  ifFalse: [
    arg_count := 0.
      Smalltalk arguments do: [:filename |
        arg_count := (arg_count + 1).
        (filename copyFrom: 1 to: 1) = '-'
        ifTrue: [
          (filename copyFrom: 2 to: 2) = 'd'
            ifTrue: [DEBUG_FLAG := true. Main_Loop.]
            ifFalse: [
              (filename copyFrom: 2 to: 2) = 't'
              ifTrue: [TREE_FLAG := true.]
              ifFalse: [
                (filename copyFrom: 2 to: 2) = 'c'
                ifTrue: [COMPRESS_FLAG := true.]
                ifFalse: [
                  (filename copyFrom: 2 to: 2) = 'u'
                    ifTrue: [UNCOMPRESS_FLAG := true.]
                    ifFalse: [
                      stderr << 'Please specify one of these options:'
                             << nl.
                      stderr << '-d: debug' << nl.
                      stderr << '-t: prints decoding tree' << nl.
                      stderr << '-c: compress' << nl.
                      stderr << '-u: uncompress' << nl.
                      stderr flush. 
                    ]
                  ]
                ]
              ]
            ]
            ifFalse: [
              arg_count = 2
                ifTrue: [
                  [in_file := FileStream open: filename
                              mode: FileStream read.
                  ] on: SystemExceptions.FileError do: [:signal |
                    |errno|
                    errno := File errno.
                    stdout flush.
                    stderr << execname << ': ' << filename << ': '
                           << signal messageText << nl.
                    stderr flush.
                  ]
                ]
                ifFalse: [
                [out_file := FileStream open: filename
                             mode: FileStream write.
                ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                    << signal messageText << nl.
                  stderr flush.
                ]
              ]
            ]
      ]
  ].



in_file = nil
  ifTrue: [in_file := stdin].

out_file = nil
  ifTrue: [out_file := stdout].


TREE_FLAG = true
  ifTrue: [
    h_tree := HuffTree new: in_file output_file: out_file opt: 't' 
      with: bStream.
    h_tree print_encoding_table.
  ].

COMPRESS_FLAG = true
  ifTrue: [
    h_tree := HuffTree new: in_file output_file: out_file opt: 'c' 
      with: bStream.
    h_tree is_file_empty ifFalse:[
      bStream := BitStream new: in_file and: out_file.
      bStream setd: 0.
      h_tree write_huffTree: bStream.
      bStream setd: 0.
      h_tree setd: 0.
      h_tree write_encoded_ver: bStream.
      bStream flushRest.
    ].
  ].

UNCOMPRESS_FLAG = true
  ifTrue: [
    bStream := BitStream new: in_file and: out_file.
    bStream setd: 0.
    h_tree := HuffTree new: in_file output_file: out_file opt: 'u' 
      with: bStream.

  ].


in_file close.
out_file close.


