(* diff: ae * string -> ae *)
exception Error of string

type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec remove x lst =
match lst with
[] -> []
| h::t -> if h = x then t
	  else h::(remove x t);;

let rec remove_zero exp = match exp with
TIMES lst -> if (List.mem (CONST 0) lst) then (CONST 0)
			   		 else (TIMES lst)
| _ -> exp


let rec pretty ae =
         match ae with
         (TIMES lst) -> (if (List.mem (CONST 0) (List.map pretty lst)) then (CONST 0) else
	                         let remove1 = (List.fold_right (fun a b -> if (a = CONST 1) then b else a::b) lst []) in
				 let remove_times = (List.fold_right (fun a b -> (match a with (TIMES lst)-> lst@b |_ -> a::b)) remove1 []) in
				 let final = (List.map pretty remove_times) in
				 (if ((List.length final) = 1) then (List.hd final) else
				 (TIMES final)))
         |(SUM lst) -> let remove0 = (List.fold_right (fun a b -> if ((pretty a) = (CONST 0)) then b else (pretty a)::b) lst []) in
                         (if (remove0 = []) then (CONST 0) else	
			 (if ((List.length remove0) = 1) then (List.hd remove0) else (SUM remove0)))
         |(POWER (a,b)) -> if (b = 0) then (CONST 1) else ae
         |_ -> ae

let rec diff (exp, x) = match exp with
	(CONST _) -> CONST 0
	| (VAR y) ->
		(if y = x then CONST 1
		else CONST 0)
	| (POWER (y, i)) ->
		(if y = x then
			if i = 1 then CONST 1
			else TIMES [CONST i ; POWER (x, i-1)]
		else CONST 0)
	| (TIMES lst) ->
		(pretty (SUM (List.map (fun y -> (remove_zero (TIMES ([diff (y, x)]@(remove y lst))))) lst)))
	| (SUM lst2) ->
		(pretty (SUM (List.map (fun y -> (diff (y, x))) lst2)))

