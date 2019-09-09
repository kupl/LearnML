(*problem 4*)
type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
	Const n -> Const 0
	|Var x -> Const 1
	|Power (x,n) ->
		(match n with
			0 -> Const 0
			|1 -> Const 1
			|_ -> Times[Const n; Power (x, n-1)])
	|Times (hd::tl) ->
		(match hd with
			|Const n -> if n=0 then Const 0 else (match tl with [] -> Const 0 |_ -> Times[Const n; diff(Times tl, x)])
			|Var x ->
				(match tl with
					[] -> Const 1
					|_ -> Times[Const 1; diff(Times tl, x)])
			|Power (x,n) ->
				(match tl with
					[] -> diff(Power(x,n),x)
					|_ -> Times[Times[Const n; Power (x,n-1)]; diff(Times tl,x)])
			|_ -> Times [diff(hd,x);diff(Times tl,x)])
	|Sum (hd::tl) ->
		(match hd with
		|Const n -> (match tl with []->Const 0 |_->diff(Sum tl,x))
		|_ -> Sum[diff (hd,x);diff (Sum tl,x)])
	|_ -> Const 0;; 