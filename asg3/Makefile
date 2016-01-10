All: hzip

hzip: hzip.st
	./hzip.st -c test.txt compressed.out
	./hzip.st -u compressed.out uncompressed.out
	diff test.txt uncompressed.out
	echo "yea"

clean: 
	- rm *.out
	echo "hello"

submit: README hzip.st tree.st BitStream.st
	submit cmps112-wm.f15 asg3 README hzip.st tree.st BitStream.st
	ls -la /afs/cats.ucsc.edu/class/cmps112-wm.f15/asg3/aaguil10

