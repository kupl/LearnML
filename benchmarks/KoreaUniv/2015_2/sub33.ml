(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with 
				| [] -> [] 
				| hd::tl -> if pred hd then hd::filter pred tl else filter pred tl

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a, b with
				| _, [] -> a 
				| [], _ -> b 
				| hd1::tl1, hd2::tl2 -> hd1::hd2::zipper(tl1,tl2);;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> let y = f in if n=0 then y else iter((n-1), f)

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
	| Sum list1 -> (match list1 with | hd::tl -> hd) (*No idea why it is not oworking*) 
	| Const x -> Const x
	| Var y -> Var y
	| Power (z1,z2) -> if z2=2 then (Times [Const z2; Var z1]) else (Times [Const (z2); Power(z1, z2-1)]);
	| Times list2 -> Const 2



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
		| INT x -> x
		| ADD (a1, b1) -> (calculator a1) + (calculator b1)
		| SUB (a2, b2) -> (calculator a2) - (calculator b2)
		| MUL (a3, b3) -> (calculator a3) * (calculator b3)
    | DIV (a4, b4) -> (calculator a4) / (calculator b4)
		| SIGMA(a5, b5, c5) -> (calculator a5)





(***)
type expression =
    | Num of int
    | Var of string
    | Let of string * expression * expression
    | Binop of string * expression * expression;;

let rec eval env = function
    | Num i -> i
    | Var x -> List.assoc x env
    | Let (x, e1, in_e2) ->
       let val_x = eval env e1 in
       eval ((x, val_x) :: env) in_e2
    | Binop (op, e1, e2) ->
       let v1 = eval env e1 in
       let v2 = eval env e2 in
       eval_op op v1 v2
  and eval_op op v1 v2 =
    match op with
    | "+" -> v1 + v2
    | "-" -> v1 - v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2
		| _ -> failwith ("Unknown operator: " ^ op);;

