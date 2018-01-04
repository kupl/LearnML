type nat = ZERO | SUCC of nat

let rec natadd(a, b) =
  match b with 
  | ZERO -> a
  | SUCC i -> natadd(SUCC(a), i)

let rec natmul(a, b) = 
  match b with
  | ZERO -> ZERO
  | SUCC i -> natadd(a, natmul(a, i))
