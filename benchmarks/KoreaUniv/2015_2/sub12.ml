(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| h::t -> if ((pred h) = true) then h::(filter pred t) else
	(filter pred t);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with 
[]-> (match b with []->[]
|h::t->(zipper(b,a)))	 
|h::t->[h]@(zipper(b,t));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
0 -> let returnf : int -> int = fun x -> x in returnf
| 1 -> f
| n -> f (fun (n-1,f));;

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
=fun (aexp,x) -> match aexp with
Const n -> Const 0
|Times [Const a ; Power (b,c)] -> if b=x then Times [Const (a*c); Power (b, (c-1))]
                                                       else Const 0
|Times [Const a; Var b] -> if b=x then Const a else Const 0
|Power (a, b) -> if a=x then Times [Const b; Power(a, (b-1))]
                            else Const 0
|Sum n -> Sum (sum n x) ;; 

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
=fun e -> 0 (* TODO *)