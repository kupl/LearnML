(* ID : 2007-12138 *)

type nat = ZERO | SUCC of nat

(*
  Main idea : 
   natadd(a,b) -> incerases b and decrease a by one every step, until a reaches ZERO
                  And when a reaches ZERO, b is the answer
*)

let rec natadd ipt = 
  match ipt with
  (ZERO,b) -> b
 |(SUCC(a),b) -> natadd (a, SUCC(b));;

