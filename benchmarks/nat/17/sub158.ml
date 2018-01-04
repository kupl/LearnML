type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a,b) ->
	match (a,b) with
	|(ZERO,ZERO)->ZERO
	|(a,ZERO)->a
	|(ZERO,b)->b
	|(SUCC(pa),SUCC(pb))->SUCC(SUCC(natadd (pa,pb)))

let rec natmul : nat * nat -> nat = fun (a,b) ->
	match (a,b) with
	|(ZERO,b)->ZERO
	|(a,ZERO)->ZERO
	|(a,SUCC(ZERO))->a
	|(a,SUCC(pb))->natadd (a,natmul(a,pb))
