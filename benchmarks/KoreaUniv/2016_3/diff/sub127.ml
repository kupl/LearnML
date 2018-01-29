
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  	match exp with 
  		| Const(a) -> Const(0)
  		| Var(a) -> if a = var then Const(1)
  					else Const(0)
  		| Power(a,b) -> if  a <> var then Const(0)
  						else if b = 1 then Const(1)
  						else Times[Const(b);Power(a,b-1)]
  		| Times(a) -> 
  			begin
  			match a with 
  				| []->Const(0)
  				| hd::tl -> Sum[Times(diff(hd,var)::tl);Times[hd;diff(Times(tl),var)]]
  			end
  		| Sum(a) ->
  			begin 
  			match a with 
  				| []->Const(0)
  				| hd::tl -> Sum[diff(hd,var);diff(Sum(tl),var)]
  			end
