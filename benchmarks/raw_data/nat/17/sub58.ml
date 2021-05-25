(* problem 2 *)
type nat = ZERO | SUCC of nat
let rec make_num t1 = match t1 with
  | ZERO -> 0
  | SUCC(x) -> if (x = ZERO) then 1 else (1 + make_num x)
let rec add = fun num -> match num with
  | 0 -> ZERO 
  | 1 -> SUCC(ZERO)
  | _ -> SUCC(add (num-1))
let natadd : nat -> nat -> nat
= fun n1 n2 -> add((make_num n1) + (make_num n2))
let natmul : nat -> nat-> nat
= fun n1 n2 -> add((make_num n1)*(make_num n2))
let rec exp = fun b n -> match n with
  | 0 -> 1
  | _ -> if (n mod 2 = 0) then (exp b (n/2))*(exp b (n/2)) else b*(exp b (n-1))
let natexp : nat -> nat -> nat
= fun n1 n2 -> add(exp (make_num n1) (make_num n2))
