type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument

let rec diff : aexp * string -> aexp = fun (ex, v) -> match (ex, v) with
| (Const a,_) -> Const 0
| (Var s, v) -> (if (s <> v) then Const 0 else Const 1)
| (Power (s, e), v) -> (if (s <> v) then Const 0 else 
		(if (e = 0) then Const 0 
		else (Times [Const e; Power (s, e-1)])))
| (Times a, v) -> (match a with 
		| [] -> raise InvalidArgument
		| [single] -> diff (single, v)
		| _ ->  Sum [Times (diff (List.hd a, v)::List.tl a) ; (Times [(List.hd a);(diff (Times (List.tl a), v))])] 
		)
| (Sum a, v) -> (match a with
		| [] -> raise InvalidArgument
		| [single] -> diff (single, v)
		| _ -> Sum [diff (List.hd a, v); diff (Sum (List.tl a), v)]
		)

