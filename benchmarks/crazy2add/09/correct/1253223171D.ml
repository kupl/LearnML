type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


let rec crazy2add((a:crazy2), (b:crazy2)) =
	let rec crazy2val((k:crazy2)) =
		match k with
 		|ZERO(k') -> 2* crazy2val (k')
 		|ONE(k') -> 1+2*crazy2val (k')
 		|MONE(k') -> -1+2*crazy2val (k') 
 		|NIL -> 0
 	in
	let sum = crazy2val(a) + crazy2val(b) in
	let rec matpl (x:int) = 
		(*
		match x with
		|0 -> ZERO NIL
		|1 -> ONE NIL
		|_ -> MONE NIL
		*)
		if x = 0 then ZERO NIL
		else if x = 1 then ONE NIL
		else if (x mod 2) = 0 then ZERO(matpl (x/2))
		else ONE(matpl (x/2))
	in
	let rec matmi (x:int) =
		if x = 0 then ZERO NIL
		else if x = 1 then MONE NIL
		else if (x mod 2) = 0 then ZERO(matmi (x/2))
		else MONE(matmi (x/2))
	in
	if(sum > 0) then matpl sum
	else matmi (-1*sum)

