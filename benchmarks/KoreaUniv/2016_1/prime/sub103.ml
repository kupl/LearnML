let rec loop = fun a b ->
 if(a mod b = 0) then false 
 else if(b=2) then true 
 else loop a (b-1) 


(* Problem 3 *)
let rec prime : int -> bool
= fun n ->  (* TODO *)
 if(n=2) then true
 else if(n=1) then false
 else loop n (n-1)
