(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> let rec loop coins amount =
match coins with
| [] -> 0
| hd::tl -> if amount < 0 then 0 
else if amount = 0 then 1
else (loop tl amount) + (loop coins (amount - hd))
in (loop coins amount)
