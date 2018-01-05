(* ID : 2007-12138 *)

let rec sigma (a,b,f) =
 if a > b then 0       (* When a>b, return 0 ; it is specified on the specification*)
 else if b = a then a              (* base case *)
 else (f b) + (sigma (a,(b-1),f))  (* inductive step *)

(* => it performs summation until b reaches a *)
