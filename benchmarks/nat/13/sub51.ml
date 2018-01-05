type nat = ZERO | SUCC of nat;;

let rec natadd (a, b) =
  match b with
  | ZERO -> a
  | SUCC pred_b-> SUCC(natadd(a, pred_b))
and natmul (a, b) =
  match b with
  | ZERO -> ZERO 
  | SUCC pred_b -> natadd(natmul(a, pred_b), a);;
  
(* test cases *)
let rec eval nat =
  match nat with
  | ZERO -> 0
  | SUCC n -> eval(n)+1;;

assert(eval(natadd(SUCC(ZERO), SUCC(ZERO)))=2);;
assert(eval(natadd(SUCC(SUCC(ZERO)), SUCC(ZERO)))=3);;
assert(eval(natmul(SUCC(SUCC(ZERO)), SUCC(SUCC(ZERO))))=4);;
assert(eval(natmul(SUCC(SUCC(SUCC(ZERO))), SUCC(SUCC(ZERO))))=6);;
assert(eval(natmul(ZERO, SUCC(SUCC(ZERO))))=0);;
assert(eval(natmul(SUCC(SUCC(ZERO)), ZERO))=0);;
