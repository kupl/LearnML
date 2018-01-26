let rec prime : int -> bool
= fun n -> 
let x = n-1 in
let rec rp : int -> bool
= fun i ->
if i = 1 then true else if n mod i = 0 then false else rp (i-1)
in if n = 1 then false else rp x;;
