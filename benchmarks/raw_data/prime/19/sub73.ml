let rec prime : int -> bool
= fun n -> (*TODO*)
pdivsr n (n-1)
and pdivsr a b =
if b<=1 then if a>=2 then true else false
else if a mod b=0 then false else true && pdivsr a (b-1);;

prime 2;;
prime 3;;
prime 4;;
prime 17;;

(* Original Code
let rec pdivsr a b = 
if b<=1 then if a>=2 then true else false
else if a mod b=0 then false else true && pdivsr a (b-1)

let prime n = pdivsr n (n-1)
*)