
type nat = ZERO | SUCC of nat

let rec cal n =
  match n with 
  1 -> SUCC ZERO
  |_ -> SUCC(cal (n-1));;
  
let natadd n1 n2 = cal (n1+n2);;
let natmul n1 n2 = cal (n1*n2);;

(*함수가 잘 작동하는지 시험*)
natadd 2 3;;
natmul 2 3;;