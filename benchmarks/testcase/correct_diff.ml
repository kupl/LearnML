{
  Sum ([Times ([Sum ([Var ("x");Var ("y")]); Var ("x")]);Var ("x")]), "x" -> 
  Sum ([Sum ([Times ([Sum ([Const (1);Const (0)]); Var ("x")]); Times ([Sum ([Var ("x");Var ("y")]); Const (1)])]);Const (1)]);
}

  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec map f l var =
  	match l with
  	| [] -> []
  	| hd::tl -> (f hd var)::(map f tl var);;

  let rec f exp var =
 	match exp with
 	| Const (n) -> Const (0)
 	| Var (x) -> if x != var then Const (0) else Const (1)
 	| Power(x, n) -> if x != var then Const (0) else Times ([Const (n); Power (x, (n-1))])
 	| Times (lst) ->
 		(
	 		match lst with
	 		| [] -> Const (0)
	 		| hd::tl -> Sum ([Times ((f hd var)::tl);Times ([hd;(f (Times (tl)) var)])])
 		)
 	| Sum (lst) -> Sum (map f lst var);;