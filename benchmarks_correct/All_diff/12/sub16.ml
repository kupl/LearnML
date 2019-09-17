(* 20008-11874 SUJEE LEE execise 2 *)

type aexp = Const of int 
| Var of string
| Power of string * int 
| Times of aexp list
| Sum of aexp list


let rec diff (aexp,str) =
	match aexp with
		| Const c -> Const 0
		| Var v -> if str = v then Const 1 else Const 0
		| Power(v,p) ->
			if v = str
			then Times [Const p; Power(v,p-1)] else Const 0
		| Times aexpl -> (* [var X; var X] check *)
			(match aexpl with
				| [] -> Times []
				| hd::tl -> Sum [ Times ((diff (hd,str))::tl) ; Times [hd; diff (Times tl,str)] ]
				)
		| Sum aexpl ->
			let diffwithstr e = diff (e,str) in
			Sum (List.map diffwithstr aexpl)
