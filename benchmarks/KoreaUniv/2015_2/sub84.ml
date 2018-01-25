(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
  [] -> []
| h::t -> if pred h then [h] @ filter pred t else [] @ filter pred t;;		

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
  [] -> [] @ b
| ah::at ->	match b with
 			  [] -> [] @ a
			| bh::bt -> [] @ [ah;bh] @ zipper (at, bt);;


(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
  0 -> fun x -> x
| _ -> fun x -> f (iter(n-1, f) x);;

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
  Const c -> Const 0
| Var v -> if v = x then Const 1 else Const 0
| Power (v, n) -> if v != x then Const 0 else Times [Const n; Power (v, n-1)]
| Times (th::tt) -> Sum [Times (diff(th, x)::tt); Times [th; diff(Times tt, x)]]
| Sum s -> Sum (List.map (fun aexp2 -> diff(aexp2, x)) s)
| Times [] -> Const 0;;

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

let rec _calc : exp * exp -> exp
=fun (e1, e2) -> match e2 with
  X -> MUL(e1, X)
| INT i -> INT i
| ADD (a, b) -> ADD (_calc(e1, a), _calc(e1, b))
| SUB (a, b) -> SUB (_calc(e1, a), _calc(e1, b))
| MUL (a, b) -> MUL (_calc(e1, a), _calc(e1, b))
| DIV (a, b) -> DIV (_calc(e1, a), _calc(e2, b))

let rec calculator : exp -> int
=fun e -> match e with
  X -> 1
| INT i -> i
| ADD (a, b) -> calculator a + calculator b
| SUB (a, b) -> calculator a - calculator b
| MUL (a, b) -> calculator a * calculator b
| DIV (a, b) -> calculator a / calculator b
| SIGMA (a, b, c) ->	if calculator a > calculator b
							then 0
						else
							calculator (_calc (a, c)) + calculator (SIGMA ((INT (calculator (ADD (a, INT 1))), b, c)));;


						  
