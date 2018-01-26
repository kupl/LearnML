
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec xcal : exp -> int -> int
  = fun exp x ->
	match exp with
	X -> x
	|INT(a) -> a
	|ADD(a, b) -> (xcal a x) + (xcal b x)
	|SUB(a, b) -> (xcal a x) - (xcal b x)
	|MUL(a, b) -> (xcal a x) * (xcal b x)
	|DIV(a, b) -> (xcal a x) / (xcal b x) 
	|_ -> 0;;

let rec sigma : int -> int -> exp -> int
= fun s e exp ->
	if s = e then xcal exp s else xcal exp s + sigma (s+1) e exp;;

  let rec calculator : exp -> int
  = fun exp -> 
	match exp with
	INT(a) -> a
	|ADD(a, b) -> (calculator a) + (calculator b)
	|SUB(a, b) -> (calculator a) - (calculator b)
	|MUL(a, b) -> (calculator a) * (calculator b)
	|DIV(a, b) -> (calculator a) / (calculator b)
	|SIGMA(a, b, c) -> sigma (calculator a) (calculator b) c
	|_ -> 0;;