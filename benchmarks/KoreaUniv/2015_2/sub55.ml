(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =  (* TODO *)
match lst with
| [] -> []
| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl
(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
= fun (a,b) -> match a,b with
| [],[] -> []
| ahd::atl,bhd::btl -> ahd::bhd::(zipper (atl, btl))
| [], b -> b
| a, [] -> a

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> match n with
| 0 -> fun x -> x
| 1 -> f
| n -> fun x -> f (iter(n-1,f) x)

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
= fun (aexp,x) -> match aexp with
  | Const c -> Const c 
  | Var v -> (match v with
    | x -> Times [Const 0;Var x])
  | Power (p,a) -> (match p,a with
    | x, a-> Times [Const a; Power (x,a-1)])
  | Times [] -> Const 1
  | Times (hd::tl) -> Times [diff(hd,x);diff(Times tl,x)]
  | Sum [] -> Const 0
  | Sum (hd::tl) -> Sum [diff(hd,x);diff(Sum tl,x)]


 

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
let calculator : exp -> int
= fun e -> 0
