
(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> 
(*if n mod 2=0 then (b fastexpt (n/2)) fastexpt 2
		else b*(b fastexpt (n-1))
*)
(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
(*
 | b=1
 | if b=1 then b+1
 | else in if n mod b =0 then b
 | 	else b+1
*)
(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> 
(* |a=0
 |if b=0 then 1
 |else if b>1 then f(b-1) + b
 |prof
*)

(* problem 5*)

let dfact : int -> int
= fun n -> (*
if n<=0 then 0
else n * dfact (n-2)
*)

(* problem 6*)

let drop : 'a list -> int -> 'a list

= fun l n -> 
(*match l with
 | [] -> raise (Failure "list is too short")
 | hd::tl -> if n<=1 then raise (Failure "not available")
 | else then hd else at (n-1) tl;;*)

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*match l with
|*)


(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
if amount<0 then 0
else if amount=0 then 1
else if amount>1
 then match coins with
 | [] -> 0
 | hd::tl -> coin
