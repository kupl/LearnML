type crazy2 = NIL
            | ONE of crazy2
						| MONE of crazy2
						| ZERO of crazy2


let rec subCrazy2add (c1 : crazy2)(c2 : crazy2)(carry : crazy2) : crazy2 =
	match c1 with
	| NIL -> 
		(match c2 with
		| NIL -> carry
		| ONE x -> 
			(match carry with
			| NIL -> ONE (subCrazy2add NIL x NIL)
			| ONE _ -> ZERO (subCrazy2add NIL x (ONE NIL))
			| MONE _ -> ZERO (subCrazy2add NIL x NIL)
			| ZERO _ -> ONE (subCrazy2add NIL x NIL) )
    | MONE x -> 
			(match carry with
			| NIL -> MONE (subCrazy2add NIL x NIL)
			| MONE _ -> ZERO (subCrazy2add NIL x (MONE NIL))
			| ZERO _ -> MONE (subCrazy2add NIL x NIL)
			| ONE _ -> ZERO (subCrazy2add NIL x NIL))
    | ZERO x -> 
			(match carry with
			| NIL -> ZERO (subCrazy2add NIL x NIL)
			| MONE _ -> MONE (subCrazy2add NIL x NIL)
			| ONE _ -> ONE (subCrazy2add NIL x NIL)
			| ZERO _ -> ZERO (subCrazy2add NIL x NIL)))
  | ONE y -> 
		(match c2 with
		| NIL -> 
			(match carry with
			| NIL -> ONE (subCrazy2add y NIL NIL)
			| ONE _ -> ZERO (subCrazy2add y NIL (ONE NIL))
			| MONE _ -> ZERO (subCrazy2add y NIL NIL)
			| ZERO _ -> ONE (subCrazy2add y NIL NIL))
    | ONE x ->
			(match carry with
			| NIL -> ZERO (subCrazy2add y x (ONE NIL))
			| ONE _ -> ONE (subCrazy2add y x (ONE NIL))
			| ZERO _ -> ZERO (subCrazy2add y x (ONE NIL))
			| MONE _ -> ONE (subCrazy2add y x NIL))
    | ZERO x ->
			(match carry with
			| NIL -> ONE (subCrazy2add y x NIL)
			| ONE _ -> ZERO (subCrazy2add y x (ONE NIL))
			| ZERO _ -> ONE (subCrazy2add y x NIL)
			| MONE _ -> ZERO (subCrazy2add y x NIL))
    | MONE x ->
			(match carry with
			| NIL -> ZERO (subCrazy2add y x NIL)
			| ONE _ -> ONE (subCrazy2add y x NIL)
			| ZERO _ -> ZERO (subCrazy2add y x NIL)
			| MONE _ -> MONE (subCrazy2add y x NIL)))
	| ZERO y ->
		(match c2 with
		| NIL -> 
			(match carry with
			| NIL -> ZERO (subCrazy2add y NIL NIL)
			| ONE _ -> ONE (subCrazy2add y NIL NIL)
			| ZERO _ -> ZERO (subCrazy2add y NIL NIL)
			| MONE _ -> MONE (subCrazy2add y NIL NIL))
    | ONE x ->
			(match carry with
			| NIL -> ONE (subCrazy2add y x NIL)
			| ONE _ -> ZERO (subCrazy2add y x (ONE NIL))
			| ZERO _ -> ONE (subCrazy2add y x NIL)
			| MONE _ -> ZERO (subCrazy2add y x NIL))
    | ZERO x ->
			(match carry with
			| NIL -> ZERO (subCrazy2add y x NIL)
			| ONE _ -> ONE (subCrazy2add y x NIL)
			| ZERO _ -> ZERO (subCrazy2add y x NIL)
			| MONE _ -> MONE (subCrazy2add y x NIL))
    | MONE x -> 
			(match carry with
			| NIL -> MONE (subCrazy2add y x NIL)
			| ONE _ -> ZERO (subCrazy2add y x NIL)
			| ZERO _ -> MONE (subCrazy2add y x NIL)
			| MONE _ -> ZERO (subCrazy2add y x (MONE NIL))))
    | MONE y ->
		(match c2 with
		 
	  | NIL -> 
			(match carry with
			| NIL -> MONE (subCrazy2add y NIL NIL)
			| ONE _ -> ZERO (subCrazy2add y NIL NIL)
			| ZERO _ -> MONE (subCrazy2add y NIL NIL)
			| MONE _ -> ZERO (subCrazy2add y NIL (MONE NIL)))
    | ONE x ->
			(match carry with
			| NIL -> ZERO (subCrazy2add y x NIL)
			| ONE _ -> ONE (subCrazy2add y  x NIL)
			| ZERO _ -> ZERO (subCrazy2add y x NIL)
			| MONE _ -> MONE (subCrazy2add y x NIL))
    | ZERO x ->
			(match carry with
			| NIL -> MONE (subCrazy2add y x NIL)
			| ONE _ -> ZERO (subCrazy2add y x NIL)
			| ZERO _ -> MONE (subCrazy2add y x NIL)
			| MONE _ -> ZERO (subCrazy2add y x (MONE NIL)))
    | MONE x ->
			(match carry with
			| NIL -> ZERO (subCrazy2add y x (MONE NIL))
			| ONE _ -> MONE (subCrazy2add y x NIL)
			| ZERO _ -> ZERO (subCrazy2add y x (MONE NIL))
			| MONE _ -> MONE (subCrazy2add y x (MONE NIL)))) 


let crazy2add (c1, c2) : crazy2 =
	subCrazy2add c1 c2 NIL
	
	
