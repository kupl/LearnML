
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec scan : string list -> string -> bool
  = fun lst s ->
	match lst with
	[] -> false
	|h::t -> if h = s then true else scan t s;;

  let rec rcheck : lambda -> string list -> bool
  = fun lambda stack -> 
	match lambda with 
	V(a) -> scan stack a
	|P(a, b) -> rcheck b (a::stack)
	|C(a, b) -> rcheck a stack  && rcheck b stack;;

let check : lambda -> bool
= fun lambda ->
	let stack = []
	in rcheck lambda stack;; 
