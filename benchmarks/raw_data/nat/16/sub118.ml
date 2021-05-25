type nat =
    	| ZERO
    	| SUCC of nat

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
    match (n1, n2) with
      |(ZERO,_) -> n2
      |(SUCC(s1),ZERO) -> (natadd s1 (SUCC ZERO))
      |(SUCC(s1),s2) -> (natadd s1 (SUCC(s2)))

let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> 
    match (n1,n2) with
      |(ZERO,_) -> ZERO
      |(_,ZERO) -> ZERO
      |(s1,(SUCC ZERO)) -> s1
      |((SUCC ZERO), s2) -> s2
      |(s1, SUCC(s2)) -> (natadd (natmul s1 s2) n1)
