(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
  | [] -> []
  | hd :: tl ->
  if f hd then hd :: filter pred tl
  else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list = 
  fun (a, b) ->
  match a with
  | [] -> b
  | hd :: tl ->
     match b with
     | [] -> a
     | hd2 :: tl2 ->
       if hd < hd2 then hd :: zipper(tl, b)
       else hd2 :: zipper(a, tl2);;


(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int) =
  fun(n, f) ->
  if n = 0 then fun x -> x
  else fun x -> iter(n-1, f) (f x);;


(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = 
fun(e, x) ->
	match e with
	| Const v -> Const 0
	| Var y -> if x = y then Const 1 else Const 0
	| Power (p, q) -> 
		if p = x && q = 1 then Const 1
		else if p = x then Times[Const q; Power(p, q-1)]
		else Const 0
	| Sum lst ->
		let rec map : aexp list -> aexp list = fun l ->
			match l with
			| [] -> []
			| hd :: tl -> diff(hd, x) :: map tl
		in Sum (map lst)
	| Times lst ->
		match lst with
		| [] -> Const 0
		| [a] -> diff(a, x)
		| hd :: tl -> Sum [Times (diff(hd, x) :: tl); Times [hd; diff(Times tl, x)]]



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

let calculator : exp -> int =
fun e ->
	let rec calc : exp -> int -> int = 
		fun ex v ->
		match ex with
		| X -> v
		| INT x -> x
		| ADD (x, y) -> (calc x v) + (calc y v)
		| SUB (x, y) -> (calc x v) - (calc y v)
		| MUL (x, y) -> (calc x v) * (calc y v)
		| DIV (x, y) -> (calc x v) / (calc y v)
		| SIGMA (a, b, ex2) ->
		if (calc a v) = (calc b v) then calc ex2 (calc a v)
		else (calc ex2 (calc a v)) + (calc (SIGMA(INT ((calc a v)+1), b, ex2)) v)
	in calc e 0

