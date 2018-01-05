type nat = ZERO | SUCC of nat

let rec ntoi (n:nat) : int =
	match n with
	| ZERO -> 0
	| SUCC n_ -> 1+(ntoi n_)

let rec iton (i:int) : nat =
	if i = 0 then ZERO
	else SUCC(iton (i-1))

let rec natadd ((nat1:nat),(nat2:nat)) : nat =
	iton((ntoi nat1)+(ntoi nat2))

let rec natmul ((nat1:nat),(nat2:nat)) : nat =
	iton((ntoi nat1)*(ntoi nat2))
