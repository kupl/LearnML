type nat = ZERO | SUCC of nat

let rec natadd (x1,x2) =
  	match (x1,x2) with
  		|(ZERO,ZERO) -> ZERO
  		|(SUCC a,ZERO) -> SUCC a
  		|(ZERO, SUCC b) -> SUCC b
  		|(SUCC a, SUCC b) -> SUCC(natadd(a,SUCC b))

let rec natmul(x1,x2) =
  	match (x1,x2) with
  		|(ZERO,_) -> ZERO
  		|(_,ZERO) -> ZERO
  		|(SUCC n1,SUCC n2) -> natadd(SUCC n1,natmul(SUCC n1, n2))