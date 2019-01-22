
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list  

 let rec map f l =
	match l with
	|[] -> []
	|hd::tl -> (f hd)::(map f tl) 
 
 let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
	match var with
	|str -> 
	(match exp with
	|Const i -> Const 0
	|Var s -> if s = str then Const 1 else Const 0
	|Power (s,i) ->
	 if s = str then (if i = 1 then Const 1
	 else Times [Const i; Power ( s, i-1)]) else Const 0
	|Times l ->
	Times l 
	|Sum l ->
	Sum (map (fun x -> diff(x, str)) l)
	|_-> raise(Failure "NotProper")