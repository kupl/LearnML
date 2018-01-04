type nat = ZERO | SUCC of nat


let rec natadd ((a:nat),(b:nat)):nat =
	match (a,b) with
	| (ZERO, bb) -> bb
	| (SUCC(aa),bb) -> natadd(aa,SUCC(bb))

let rec natmul ((a:nat),(b:nat)):nat =
	match (a,b) with
	| (ZERO, bb) -> ZERO
	| (SUCC(aa),bb) -> natadd(bb,natmul(aa,bb))