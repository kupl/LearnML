
	type aexp = 
	  | Const of int
	  | Var of string
	  | Power of string * int
	  | Times of aexp list
	  | Sum of aexp list
	;;

	let rec diff : aexp * string -> aexp
	  = fun (exp, var) -> 
	    match exp with
	    | Const(_) -> Const 0
	    | Var(x) ->
	      if x = var
	      then Const 1
	      else Var x
	    | Power(x, const) ->
	      if x = var
	      then Times [Const const; Power(x, const - 1)]
	      else Power(x, const)
	    | Times(list) ->
	      begin
	      match list with
	      | [] -> Const 1
	      | hd::tl -> Sum[Times(diff(hd, var) :: tl); diff(Times(tl), var)]
	      end
	    | Sum(list) ->
	      begin
	      match list with
	      | [] -> Const 0
	      | hd::tl -> Sum[diff(hd, var); diff(Sum(tl), var)]
	      end