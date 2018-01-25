(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*fastexpt b (n-1);;

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec init n i =
if i*i > n then n
else if (n mod i = 0) then i
else init n (i+1) in 
init n 2;;

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 0 then fun x -> x
else fun x -> f(iter(n-1, f) x);;


(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a
else if a < b then (f a)*(product f (a+1) b)
else (f a)*(product f (a-1) b);;

(* problem 5*)
let rec dfact : int -> int
= fun n -> if n = 1 || n = 2 then n
else n*dfact(n-2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> if n = 0 then l
else if List.length l = 0 then []
else drop (match l with hd::tl -> tl) (n-1);;
	
(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with 
| [] -> ([], [])
| (x,y) :: tp ->
	let tpl = unzip tp in
	((x::(fst tpl)), (y::(snd tpl)))

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> if List.length coins > 0 && amount = 0 then 1
else if amount < 0 || (List.length coins = 0 && amount > 0) then 0
else match coins with 
| [] -> 0
| hd :: tl -> (change tl amount) + (change coins (amount - hd));;



