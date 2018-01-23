type nat = ZERO | SUCC of nat;;

let rec natadd (a, b) =
  match b with
  | ZERO -> a
  | SUCC pred_b-> SUCC(natadd(a, pred_b))
and natmul (a, b) =
  match b with
  | ZERO -> ZERO 
  | SUCC pred_b -> natadd(natmul(a, pred_b), a);;
  

