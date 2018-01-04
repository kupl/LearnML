(*hw1-6 컴퓨터 공학부 2008-11641 신희식*) 

type nat = ZERO | SUCC of nat

let rec natadd tuple_of_nat =
	match tuple_of_nat with
	(ZERO, ZERO) ->
		ZERO	
	|(ZERO, (SUCC nat1)) ->
		(SUCC nat1)
	|((SUCC nat1), ZERO) ->
		(SUCC nat1)
	|((SUCC nat1), (SUCC nat2)) ->
		(SUCC (natadd (nat1, (SUCC nat2))))

let rec natmul tuple_of_nat =
	match tuple_of_nat with
	((SUCC nat1), (SUCC nat2)) ->
		(if (nat2 = ZERO) then
		 (SUCC nat1)
		 else
		 (natadd ((SUCC nat1), (natmul ((SUCC nat1), nat2))))
		)
	|(_,_) ->
		ZERO
	
