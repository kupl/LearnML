type nat= ZERO | SUCC of nat


 

let rec natadd (n1, n2) =
	match (n1, n2) with
		|(ZERO, ZERO) -> ZERO
		|(SUCC nn1, SUCC nn2) -> SUCC (SUCC (natadd(nn1, nn2)))
		|(SUCC n, ZERO) ->  SUCC n
		|(ZERO, SUCC n) ->  SUCC n

let rec natmul (n1, n2) =
	match (n1, n2) with
		|(ZERO, ZERO) -> ZERO
		|(SUCC nn1, SUCC nn2) -> natadd ( natmul(nn1, nn2) , natadd(nn1,SUCC nn2))
		|(SUCC n, ZERO) ->  ZERO
		|(ZERO, SUCC n) ->  ZERO

