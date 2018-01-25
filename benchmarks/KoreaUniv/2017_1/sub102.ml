(* problem 1*)
let square x = x * x

let rec fastexpt : int -> int -> int
= fun b n ->
match n with
  0 -> 1
| 1 -> b
| _ -> match n mod 2 with 
      | 0 -> square(fastexpt b (n/2))
      | _ -> square(fastexpt b (n/2)) * b

(* problem 2*)

let rec find_div : int -> int -> int
= fun n x ->
if n mod x = 0 then x
else if x*x > n then n
else find_div n (x+1)

let smallest_divisor : int -> int
= fun n -> find_div n 2

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
match n with
  0 -> (fun x -> x)
| _ -> (fun x -> f( iter((n-1),f) x ) ) 

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then (f a)
else (f a) * (product f (a+1) b)

(* problem 5*)

let rec dfact : int -> int
= fun n -> 
if n < 3 then n
else n * dfact (n-2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
if n = 0 then l
else match l with
  [] -> []
| hd::tl -> drop tl (n-1)

(* problem 7*)
let fst (x,_) = x
let snd (_,x) = x

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
  [] -> ([],[])
| hd::tl -> let tup = unzip tl in
   (fst(hd)::fst(tup) , snd(hd)::snd(tup))

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
if amount < 0 then 0
else if amount = 0 then 1
else 
match coins with
  [] -> 0
| hd::tl -> change coins (amount-hd) + change tl amount
