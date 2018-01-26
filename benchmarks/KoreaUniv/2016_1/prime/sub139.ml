let prime : int -> bool
= fun n ->  (* TODO *)
let rec prime2(n,x) = 
if x=n then true
else if n mod x=0 then false
else prime2 (n,x+1)
in prime2(n,2);;
