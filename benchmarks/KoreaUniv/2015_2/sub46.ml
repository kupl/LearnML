(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
	match lst with
	|[] -> []
	|hd::tl -> if pred hd = true then hd::(filter pred tl) else filter pred tl;;




(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match (a,b) with
							|([],[]) -> []
							|([],b) -> b
							|(a,[]) -> a
							|(hda::tla, hdb::tlb) -> hda::(hdb::zipper(tla, tlb));;


(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
							|0 -> fun x -> x
							|n -> fun x -> iter(n-1, f) (f x) ;;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec map f l =
	match l with
	|[] -> []
	|hd::tl -> (f hd)::(map f tl);;



let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
								|Sum l ->Sum (match l with
															|[] -> [Const 0]
															|hd::tl -> diff(hd,x)::[diff(Sum tl,x)])

								|Const n -> Const 0
								|Var x -> Const 1
								|Power (x, n) -> (match n with
																	|1 -> Const 1;
																	|n -> Times[Const n;Power(x,n-1)])
								|Times l -> (match l with
															|[]->Const 0
															|[Const a; Const b; ax] -> Times [Const (a*b); diff(ax, x)]
															|[Const a; ax] -> Times [Const a; diff(ax,x)]
															|[Const a] -> Const 0
															|[ax] -> diff(ax,x));;

														



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


let rec calculator2
=fun e ->  match e with
						|X -> 1 (*wrong*)
						|INT n -> n
						|ADD(a,b) -> (calculator2 a)+(calculator2 b)
						|SUB(a,b) -> (calculator2 a)-(calculator2 b)
						|MUL(a,b) -> (calculator2 a)*(calculator2 b)
						|DIV(a,b) -> (calculator2 a)/(calculator2 b)
						|SIGMA(a,b,c) -> if (calculator2 a) > (calculator2 b) then 0 else (calculator2 c) + calculator2( SIGMA (INT ((calculator2 a)+1),b,c));;

let calculator : exp -> int
= fun e -> calculator2(e);;
