(*********************)
(* Problem 1: filter *)
(*********************)
 
let rec filter pred lst = 
match lst with
|[]->[]
|h::t-> if pred h then h::filter pred t else filter pred t;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match (a,b) with 
|([],[])->[]
|(a, [])->a
|([], b)->b
|(h1::t1, h2::t2) -> h1::h2::zipper(t1, t2);;


(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->match n with 
|0 -> fun y->y
|1 -> f
|_ ->iter(n-1,fun x->f (f x) );;

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
=fun (aexp,x) ->match aexp with 
|Const _ -> Const 0
|Var y -> if x=y then Const 1 else Const 0
|Power(y, n)-> if x=y then Times [Const n; Power(x, (n-1))] else Const 0
|Times (h::t)->Sum[ Times(diff(h,x)::t) ; Times[h;diff(Times t,x)] ]
|Times []->Const 0
|Sum lsts -> Sum(List.map (fun z->diff(z,x)) lsts);;
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

let rec calculator : exp -> int
=fun e -> match e with 
    X->raise (Failure "Unvalid expression")
    |INT (x) -> x
    |ADD (x, y) -> calculator(x) + calculator(y)
    |SUB (x, y) -> calculator(x) - calculator(y)
    |MUL (x, y) -> calculator(x) * calculator(y)
    |DIV (x, y) -> calculator(x) / calculator(y)
    |SIGMA (x,y,z) -> exp_to_func(e, x)

and exp_to_func : (exp * exp) -> int
=fun(a, b) -> match a with 
    |X-> calculator (b)
    |INT (x) -> x
    |ADD (x, y) -> exp_to_func(x, b) + exp_to_func(y, b)
    |SUB (x, y) -> exp_to_func(x, b) - exp_to_func(y, b)
    |MUL (x, y) -> exp_to_func(x, b) * exp_to_func(y, b)
    |DIV (x, y) -> exp_to_func(x, b) / exp_to_func(y, b)
    |SIGMA (x,y,z) -> if(calculator(x) <= calculator(y)) then exp_to_func (z, x) + calculator( SIGMA( INT(calculator (ADD(x, INT 1))), y, z) ) else 0



