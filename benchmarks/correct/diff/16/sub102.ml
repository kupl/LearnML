
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list 


  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
		match exp with
		| Const num-> Const 0
		| Var v -> if v=var then Const 1 else Const 0
		| Power (v,p) -> 
			if v <> var then Const 0
			else begin
				if p = 0 then Const 0
				else if p = 1 then Const 1
				else Times([Const p ; Power (v, p-1)])
			end
		| Times lst  ->
			begin
				match lst with
				| hd::tl ->  if tl=[] then diff(hd,var)
                     else Sum([Times(diff(hd,var)::tl);Times([hd;diff(Times(tl),var)])])
				| _ -> Const 1
			end
		| Sum lst ->
			begin	
				match lst with
			 |		hd::tl ->  Sum ([diff(hd,var);diff(Sum(tl),var)])
			 | _ -> Const 0
			end