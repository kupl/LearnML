(* 2014-19180 You JooSeung Question 4*)

type nat = ZERO | SUCC of nat

let rec natadd((n1:nat),(n2:nat)):nat = match n1 with
  | ZERO -> n2
  | SUCC(n1_minus_1) -> natadd(n1_minus_1, SUCC(n2))

let rec natmul((n1:nat),(n2:nat)): nat = match n1 with
  | ZERO -> ZERO
  | SUCC(ZERO) -> n2
  | SUCC(n1_minus_1) -> natadd(n2, natmul(n1_minus_1,n2))
