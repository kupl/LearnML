exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add ((a: crazy2) , (b: crazy2)): crazy2 =
	let rec integerize (c: crazy2): int list =
		match c with 
		| NIL -> []
		| ZERO c2 -> 0::(integerize c2)
		| ONE c2 -> 1::(integerize c2)
		| MONE c2 -> -1::(integerize c2)
	in
	
	let rec add li1 li2 =
		match li1 with 
		| [] -> li2
		| hd1::tl1 -> (match li2 with 
				  	  | [] -> li1
					  | hd2::tl2 -> (hd1 + hd2)::(add tl1 tl2))
	in

	let rec carry li =
		match li with
		| [] -> []
		| fst::[] -> if fst < -1 then (fst + 2)::[-1]
						else if fst > 1 then (fst - 2)::[1]
							else [fst]
		| fst::snd::tl -> if fst < -1 then (fst + 2)::(carry ((snd - 1)::tl))
							else if fst > 1 then (fst - 2)::(carry ((snd + 1)::tl))
								else fst::(carry (snd::tl))
	in

	let rec crazify (li: int list) : crazy2 =
		match li with
		| [] -> NIL
		| hd::tl -> (match hd with
					| 0 -> ZERO (crazify tl)
					| 1 -> ONE (crazify tl)
					| -1 -> MONE (crazify tl)
					| _ -> NIL)
	in

	(crazify (carry (add (integerize a) (integerize b))))
	






