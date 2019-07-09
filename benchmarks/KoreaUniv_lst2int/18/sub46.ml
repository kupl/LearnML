let rec ten n = if n = 1 then 1 else 10 * ten (n-1);;
let rec rangeint n a = if (n - ten a) < 0 then a - 1 else rangeint n (a + 1);;
let rec range lst = match lst with
  | [] -> 0
  | hd::tl -> let x = rangeint hd 2 in if x > 1 then x + (range tl) else (range tl) + 1;;


let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> let x = rangeint hd 2 in 
    if x = 1 then (ten (range lst)) * hd + lst2int tl else (ten ((range lst) - x + 1)) * hd + lst2int tl;;

(*
lst2int [2;3;4;5];;
lst2int [1;2163;3;6387;1;23;521];;
*)