(* diff: aexp * string -> aexp *)
exception Error of string

type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec remove x lst =
match lst with
[] -> []
| h::t -> if h = x then t
	  else h::(remove x t);;

let rec remove_zero exp = match exp with
Times lst -> if (List.mem (Const 0) lst) then (Const 0)
			   		 else (Times lst)
| _ -> exp


let rec pretty aexp =
         match aexp with
         (Times lst) -> (if (List.mem (Const 0) (List.map pretty lst)) then (Const 0) else
	                         let remove1 = (List.fold_right (fun a b -> if (a = Const 1) then b else a::b) lst []) in
				 let remove_times = (List.fold_right (fun a b -> (match a with (Times lst)-> lst@b |_ -> a::b)) remove1 []) in
				 let final = (List.map pretty remove_times) in
				 (if ((List.length final) = 1) then (List.hd final) else
				 (Times final)))
         |(Sum lst) -> let remove0 = (List.fold_right (fun a b -> if ((pretty a) = (Const 0)) then b else (pretty a)::b) lst []) in
                         (if (remove0 = []) then (Const 0) else	
			 (if ((List.length remove0) = 1) then (List.hd remove0) else (Sum remove0)))
         |(Power (a,b)) -> if (b = 0) then (Const 1) else aexp
         |_ -> aexp

let rec diff (exp, x) = match exp with
	(Const _) -> Const 0
	| (Var y) ->
		(if y = x then Const 1
		else Const 0)
	| (Power (y, i)) ->
		(if y = x then
			if i = 1 then Const 1
			else Times [Const i ; Power (x, i-1)]
		else Const 0)
	| (Times lst) ->
		(pretty (Sum (List.map (fun y -> (remove_zero (Times ([diff (y, x)]@(remove y lst))))) lst)))
	| (Sum lst2) ->
		(pretty (Sum (List.map (fun y -> (diff (y, x))) lst2)))

