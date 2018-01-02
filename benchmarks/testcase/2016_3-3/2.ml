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
	|X -> x
	|INT(a) -> a
	|ADD(a, b) -> (xcal a x) + (xcal b x)
	|SUB(a, b) -> (xcal a x) - (xcal b x)
	|MUL(a, b) -> (xcal a x) * (xcal b x)
	|DIV(a, b) -> (xcal a x) / (xcal b x) 
	|_ -> 0;;

let rec sigma : int -> int -> exp -> int
= fun s e exp ->
	if s = e then xcal exp s else (xcal exp s) + (sigma (s+1) e exp);;

  let rec f : exp -> int
  = fun exp -> 
	match exp with
	|INT (a) -> a
	|ADD (a, b) -> (f a) + (f b)
	|SUB (a, b) -> (f a) - (f b)
	|MUL (a, b) -> (f a) * (f b)
	|DIV (a, b) -> (f a) / (f b)
	|SIGMA (a, b, c) -> sigma (f a) (f b) c
	|_ -> 0;;