(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 then 1 else fib(n-1) + fib(n-2);;



(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 = n2 || n2 = 0 then 1
                  else pascal(n1, n2 - 1)*(n1 - n2 + 1)/ n2;;


(* Problem 3 *)
let rec prime : int -> bool
= fun n -> if n%prime(n-1) = 0 then false else true;;



(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then 0 
               else if f=f then sigma(f,a+1,b) 
               else sigma(f a,a+1,b);;
