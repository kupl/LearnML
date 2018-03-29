(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
if amount<0 then 0
else if amount=0 then 1
else if amount>1
 then match coins with
 | [] -> 0
 | hd::tl -> coin
