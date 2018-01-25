(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
  [] -> []
  |hd::tl -> if (pred hd) then hd::(filter pred tl)
            else (filter pred tl) 



(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
        match a,b with
        |[], [] -> []
        |a, [] -> a
        |[],b -> b 
        |hd1::tl1 , hd2::tl2 -> hd1::hd2::(zipper (tl1,tl2)) 

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
        if (n<0) then raise (Failure "error") else
        match n with 
        | 0 -> ( fun x -> x)
        | n -> ( fun x -> f (iter(n-1,f) x))

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
match aexp with
| Const n -> Const 0
| Var x -> Const 1
| Power (x,n) -> (match n with 
                  0 -> Const 1
                  |_ -> Times [Const n; diff (Power (x,n-1),x)]) 
| Times (a::b::tl) -> Times [ a; diff (b,x)]
| Sum (hd::tl) -> ( match hd with
                   
                  |Const 0  -> Const 0
                  |_ -> Sum [diff(hd,x); diff (Sum tl, x)])
|Times [] -> raise (Failure "error")  


(*************************)
(* Problem 5: Calculator *)
(*************************)
