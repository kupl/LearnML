(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
else if n mod 2 = 1 then b * (fastexpt b(n-1))
  else (fastexpt b(n/2))*(fastexpt b(n-2));;

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec doo n i = 
  if i>n then n else
  if (n mod i = 0) then i 
  else doo n(i+1) in 
  doo n 2;;

(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n=0 then f
else fun x -> iter(n-1 f)(f x);;

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> (*TODO*)

(* problem 5*)

let dfact : int -> int
= fun n -> (*TODO*)

(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> match | with
[] -> if n<=0 
then [] else 
| h::t -> 
if n = 0 then l else drop (n-1) t;;

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
