let rec pr = fun (n,i) -> if n mod i =0  then false
else if i = 2 then true
else pr(n,i-1)
let rec prime : int -> bool
= fun n -> if n=2 || n=3 then true
else pr (n, n/2)
