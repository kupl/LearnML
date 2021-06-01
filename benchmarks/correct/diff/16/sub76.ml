
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	Const(i) -> Const(0)
	|Var(str) -> if(var = str) then Const(1) else Const(0)
	|Power(str,i) -> if(var = str) then Times([Const(i);Power(str,i-1)]) else Const(0)
	|Sum(l2) -> begin
		match l2 with
		[] -> Const(0)
		|hd::tl -> Sum([diff (hd,var);diff (Sum(tl),var)])
		end
	|Times (lst) -> begin
		match lst with
		[] -> Const(0)
		|hd::tl -> Sum([Times((diff (hd,var))::tl);Times([hd;diff (Times(tl),var)])])
		end (* TODO *)