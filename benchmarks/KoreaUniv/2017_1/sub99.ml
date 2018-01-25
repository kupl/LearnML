(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n < 0 then raise(Failure"Negative") else match n with 
|0 -> 1
|1 -> b
|_ -> if(n mod 2 = 0) then (fastexpt b (n/2)) * (fastexpt b (n/2)) else b * (fastexpt b (n-1))

(* problem 2*)

let rec smallest_divisor : int -> int
= fun n -> if n <= 1 then raise(Failure "Error") else
let rec sml n x = match (n mod x) with
|0 -> x
|_ -> if(x * x > n) then n else sml n (x+1) in
 sml n 2

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> match n with
|0 -> (fun x->x)
|_ -> if n < 0 then raise(Failure "Error") else fun x -> iter((n-1),f)(f(x))

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a > b then raise(Failure "Error") else 
match (b-a) with
|0 -> f a
|_ -> (f a) * product f (a+1) b

(* problem 5*)

let rec dfact : int -> int
= fun n -> if n < 0 then raise(Failure "Negative") else match (n-1)/2 with
|0 -> n
|_ -> n * dfact (n-2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
|[] -> []
|h::t -> if n <= 0 then h::t else (drop t (n-1))

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec mkl1 list = match list with
|[] -> []
|(x,y)::tl -> x:: (mkl1 tl)
  in let rec mkl2 list = match list with
  |[] ->[]
  |(x,y)::tl -> y:: (mkl2 tl)
   in ((mkl1 lst),(mkl2 lst))

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
let rec insert a l = match l with
|[] -> [a]
|hd::tl -> if a > hd then a::hd::tl else hd::insert a tl in
 let rec sort l = match l with
 |[] -> []
 |hd::tl -> insert hd (sort tl) in
  let rec sub l a = match l with
  |[] -> 0
  |hd::tl -> match a with
   |0 -> 1
   |_ -> if a < 0 then 0 else sub l (a-hd) + sub tl a
   in sub (sort coins) amount;;

