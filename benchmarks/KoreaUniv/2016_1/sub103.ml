(* Problem 1 *)
let rec fib : int -> int
= fun n -> (* TODO *)

match n with
|0 -> 0
|1 -> 1
|n -> fib(n-1) + fib(n-2)




(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->  (* TODO *)

if (n2=0 || n1=n2) then 1
else pascal(n1-1, n2) + pascal(n1-1, n2-1)


  

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



(*Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> (* TODO *)

if (a<=b) then f a + (sigma (f) (a+1) b)
else 0
