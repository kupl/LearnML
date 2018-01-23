type nat = ZERO | SUCC of nat

let rec natadd(x,y) =
  match y with
  |ZERO -> x
  |SUCC y -> natadd(SUCC x,y)
 
let rec natmul(x,y) = 
  match y with
  |ZERO -> ZERO
  |SUCC y -> natadd(y,natmul(x,y))

