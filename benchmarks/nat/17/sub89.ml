
type nat = ZERO | SUCC of nat

(** natadd : nat * nat -> nat *)
let rec natadd (x,y) = match y with
  | ZERO -> x (* x+0 = x *)
  | SUCC z -> SUCC (natadd (x, z)) (* x+ (z+1) = (x+z) + 1  *)


(** natmul : nat * nat -> nat *)
let rec natmul (x, y) = match y with
  | ZERO -> ZERO (* x*0 = 0 *)
  | SUCC z -> natadd(natmul(x, z) , x) (* x*(z+1) = (x*z) + x  *)
