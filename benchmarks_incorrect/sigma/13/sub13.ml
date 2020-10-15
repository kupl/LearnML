(* ID : 2007-12138 *)

let rec sigma f a b =
 if a > b then 0       (* When a>b, return 0 ; it is specified on the specification*)
 else if b = a then a              (* base case *)
 else (f b) + (sigma f a (b-1))  (* inductive step *)

(* => it performs summation until b reaches a *)
