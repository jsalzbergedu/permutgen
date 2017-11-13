all:
	csc -O3 permutgen.scm
windows:
	csc
	csc -deploy 
