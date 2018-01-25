(* problem 1*)
let rec fastexpt : int -> int -> int
  = fun b n -> 
    if n = 0 then 1 
    else if n = 1 then b
    else let a = fastexpt b (n/2) in a*a*
                                     (if n mod 2 == 0 then 1 else b);;

(* problem 2*)
let smallest_divisor : int -> int
  = fun n -> 
    if n <= 2 then n
    else
      let rec test n2 i = 
        if i*i > n2 then n2
        else if n2 mod i = 0 then i
        else test n2 (i+1) in test n 2;;

(* Problem 3 *)
let rec iter : int * (int -> int) -> (int -> int)
  = fun (n,f) -> 
    if n>0
    then (fun x -> (let f2 = iter (n-1,f) in f2(f x)))
    else fun x -> x;;

(* Problem 4 *)
let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> 
    if(a > b) then 1
    else (f a) * (product f (a+1) b);;

let fact n = product (fun x -> x) 5 n in fact 10;;

(* problem 5*)
let rec dfact : int -> int
  = fun n -> 
    if n <= 1 then 1
    else n * (dfact (n-2));;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
  = fun l n ->
    if n = 0 then l
    else match l with
        [] -> l
      | h::t -> drop t (n-1);;







