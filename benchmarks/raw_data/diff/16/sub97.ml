
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
	
	
	let rec length : aexp list -> int 
	=fun lst -> match lst with
	| [] -> 0
	|	hd::tl -> 1 + length tl

  let diff : aexp * string -> aexp
  = fun (exp, var) -> 
  let rec diff_exp : aexp * string -> aexp 
  = fun (exp, var) -> 
  match exp with
  | Const n -> Const 0
  | Var value -> if var=value then Const 1 else Const 0
  | Power (value, n) -> if var=value then Times [Const n; Power (value, n-1)] else Const 0  
	| Sum lst ->
		begin
	  let rec sum_diff : aexp list * string -> aexp list
		= fun(sum_lst, key) ->
		match sum_lst with
		|[] -> []
		|hd::tl -> diff_exp(hd, key)::(sum_diff(tl, key))
		in Sum(sum_diff(lst, var))
		end
	| Times lst ->
		let rec filter : aexp list * string * int * int -> aexp list
		= fun(lst, key, p, q) ->
		match lst with
		| [] -> []
		| hd::tl -> if p==q then diff_exp(hd, key)::filter(tl, key, p+1, q)
			else hd::filter(tl, key, p+1, q)
		in let rec time_diff : aexp list * string * int * int-> aexp list
		= fun(time_lst, key, x, y) -> if x<=y then Times(filter(time_lst, key, 1, x))::time_diff(time_lst, key, x+1, y) else []
		in Sum(time_diff(lst, var, 1, (length lst)))
	in diff_exp(exp, var)