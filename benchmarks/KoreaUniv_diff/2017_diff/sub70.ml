(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 

	let rec diff_calculation : aexp * string -> aexp
	= fun (e,x) ->
		match e with
		| Const a ->Const 0
		| Sum(l) -> 
			begin
				match l with
				| [] -> Sum[]
				| [a] -> diff_calculation (a,x)
				| hd::tl -> 
					begin
						match hd with
						| Const _-> (diff_calculation(Sum(tl),x))
						| _ -> Sum(List.append [(diff_calculation(hd,x))] [(diff_calculation(Sum(tl),x))])
					end
			end
		| Power(a,b) ->
			if x=a then 
				begin
					if(b==1) then Const 1
					else Times[Const b;Power(a,b-1)]
				end
			else Const 0
		| Times l ->
			begin
				match l with
				| [] -> Const 0
				| [a] -> diff_calculation(a,x)
				| hd::tl -> 
					begin
						match hd with
						| Const 0 -> Const 0
						| _ -> Sum[Times[hd;diff_calculation(Times(tl),x)];Times[diff_calculation(hd,x);Times(tl)]]
					end
			end
		| Var a -> if x=a then Const 1 else Const 0
	in

	let rec beautiful_diff : aexp -> aexp
	= fun e->
		match e with
		| Const(a) -> Const(a)
		| Var(a) -> Var(a)
		| Power(a,b) -> Power(a,b)
		| Times(l)->
			begin
				match l with
				| [] -> Const 0
				| [a] -> beautiful_diff a
				| hd::tl -> 
					begin
						match hd with
						| Times([]) -> Times([beautiful_diff (Times(tl))])
						| Sum([]) -> Times([beautiful_diff (Times(tl))])
						| Const 1 -> Times([beautiful_diff (Times(tl))])
						| Const 0 -> Const 0
						| _ -> Times(List.append [hd] [(beautiful_diff (Times(tl)))])
					end
			end
		| Sum(l) ->
			begin
				match l with
				| [] -> Const 0
				| [a] -> beautiful_diff a
				| hd::tl ->
					begin
						match hd with
						| Times([]) -> Sum([beautiful_diff (Times(tl))])
						| Sum([]) -> Sum([beautiful_diff (Times(tl))])
						| Const 0 -> Sum([beautiful_diff (Times(tl))])
						| _ -> Sum(List.append [hd] [(beautiful_diff (Times(tl)))])
					end
			end
	in

	beautiful_diff(diff_calculation(e,x))
