
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
		match exp with
		| Const n -> Const 0
		| Var x -> if x = var then Const 1 else Const 0
		| Power (x, n) ->
			if x = var then
				begin
				if n = 1 then Const n
				else Times [Const n; Power (x, n-1)]
				end
			else Const 0
		| Times lst ->
			let rec length l =
				begin
				match l with
				| [] -> 0
				| hd::tl -> 1 + length tl
				end
				in
				let rec check1 : aexp list -> int -> aexp list
				= fun lst' n ->
					begin
					match lst' with
					| [] -> []
					| hd::tl -> if n = 0 then [diff (hd, var)]@tl else hd::(check1 tl (n-1))
					end
				in
				let rec check2 : aexp list -> int -> aexp list
				= fun lst' n -> if n = (length lst' - 1) then [Times (check1 lst' n)] else [Times (check1 lst' n)]@(check2 lst' (n+1))
					in Sum (check2 lst 0)
		| Sum lst ->
			let rec check : aexp list -> aexp list
			= fun lst' ->
				begin
				match lst' with
				| [] -> []
				| hd::[] -> [diff (hd, var)]
				| hd::tl -> [(diff (hd, var))]@(check tl)
				end
			in Sum (check lst)