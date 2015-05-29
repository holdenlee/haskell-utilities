Scripts
=======

hc compiles the file.

	ghc -o $1 $1.hs

The file should be named A.hs, B.hs, C.hs, etc.

r runs the file on small input. (ex. r A) On the first (0th) attempt, omit the 2nd parameter. On subsequent attempts, add the number as the second argument. (ex. r A 1)

	num=${2-"0"}
	./$1 "$1-small-attempt$num.in"

rb runs the file on large input.

./$1 "$1-large.in"
