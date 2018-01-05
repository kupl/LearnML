type crazy2 = NIL
              | ZERO of crazy2
							| ONE of crazy2
							| MONE of crazy2


let rec subCrazy2val (c, k) : int =
	match c with
	| NIL -> 0
	| ONE x -> k + subCrazy2val (x, 2*k)
	| MONE x -> -1*k + subCrazy2val (x, 2*k)
	| ZERO x -> subCrazy2val (x, 2*k)


let crazy2val (c : crazy2) : int =
	subCrazy2val (c, 1)