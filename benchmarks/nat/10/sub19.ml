
(*Ex6*)
type nat = ZERO | SUCC of nat

let rec natadd:nat*nat->nat = fun (a, b) ->
	match a with ZERO -> b
			|	SUCC aa -> SUCC (natadd(aa,b))

let rec natmul:nat * nat->nat = fun (a, b) ->
	match a with ZERO -> ZERO
			|	SUCC aa -> natadd(b, (natmul(aa,b)))
