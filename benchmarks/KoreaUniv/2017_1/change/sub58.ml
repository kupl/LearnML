(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> if amount < 0 then 0
  else if amount = 0 then 1
  else match coins with
  | [] -> 0
  | hd::[] -> if amount mod hd = 0 then 1 else 0
  | hd::tl -> if amount < hd then 0
  else (change (hd::tl) (amount-hd)) + (change tl amount);;

