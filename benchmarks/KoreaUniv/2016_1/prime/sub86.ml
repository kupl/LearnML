let rec prime : int -> bool
= fun n ->
let rec inpr : int -> bool
= fun k -> 
if k = n then true else if n mod k = 0 then false else inpr (k+1)  
in inpr 2;; (* TODO *)
