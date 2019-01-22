
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec scan : string list -> string -> bool
  = fun lst s ->
	match lst with
	[] -> false
	|h::t -> if h = s then true else scan t s;;

  let rec rcheck : exp -> string list -> bool
  = fun exp stack -> 
	match exp with 
	V(a) -> scan stack a
	|P(a, b) -> rcheck b (a::stack)
	|C(a, b) -> rcheck a stack  && rcheck b stack;;

let check : exp -> bool
= fun exp ->
	let stack = []
	in rcheck exp stack;; 
