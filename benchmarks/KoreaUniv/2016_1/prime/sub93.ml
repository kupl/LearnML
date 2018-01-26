let rec prime2 a b
= if a=1 then false
else if a=b then true
else if (a mod b)=0 then false
else prime2 a (b+1)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> prime2 n 2
