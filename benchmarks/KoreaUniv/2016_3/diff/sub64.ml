
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
        |Const a -> Const 0
        |Var x -> if x = var then Const 1 else Var x
        |Power (s, i) -> 
            if s = var then Times[Const i;Power (s, i - 1)] else Power (s, i)
	    |Times l -> begin
 	                    match l with
   	                    |h::t -> Sum[Times[diff (h, var);t];diff (t, var)]
      		        end
    	|Sum m ->
      		 match m with
       		  |h::t -> Sum[diff(h, var); diff(t, var)] (* TODO *)