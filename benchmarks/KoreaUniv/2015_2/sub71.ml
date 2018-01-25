(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| hd::tl -> if (pred hd) then hd::(filter pred tl)
else (filter pred tl);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a,b with
| a,[] -> a
| [],b -> b
| hda::tla,hdb::tlb -> if (hda < hdb) then hda::(zipper (tla,b))
else hdb::(zipper (a,tlb));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
| 0 -> (fun x -> x)
| n -> (fun x -> f (iter(n-1,f) x));;


(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec isvarpow : aexp -> bool
=fun aexp -> match aexp with
| Var(t) -> true
| Power(x,t) -> true
| _ -> false;;

let rec istherevarpow : (aexp list) -> bool
=fun l -> match l with
| [] -> false
| hd::tl -> if (isvarpow hd)
then true
else istherevarpow(tl);;

let aexptolist : aexp -> aexp list
= fun aexp -> match aexp with
| Times (hd::tl) -> hd::tl
| Sum (hd::tl) -> hd::tl
| _ -> [];;

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
| Const a -> Const 0
| Var t -> if t = x then Const 1 else Const 0
| Power (t,n) -> if t = x
then Times [Const n ; Power(t,n-1)]
else Const 0
| Times (hd :: tl) ->
Sum ( Times(diff(hd,x)::tl) :: aexptolist(Times(hd::aexptolist(diff(Times(tl),x)))))
| Sum (hd::tl) -> Sum (diff(hd,x) :: aexptolist(diff(Sum(tl),x)));;



(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal : (int -> int -> int) -> exp -> exp -> int
=fun op e1 e2 ->
let v1 = calculator e1 in
let v2 = calculator e1 in
(match v1, v2 with
| INT n1, INT n2 -> INT (op n1 n2)
| _ -> raise (Failure "ERROR: X is not defined"));;


let calculator : exp -> int
=fun e -> match e with
| INT a -> a
| ADD(e1,e2) -> cal (+) e1 e2
| SUB(e1,e2) -> cal (-) e1 e2
| MUL(e1,e2) -> cal (*) e1 e2
| DIV(e1,e2) -> cal (/) e1 e2