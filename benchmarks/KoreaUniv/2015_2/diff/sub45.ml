type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
	match aexp with
	| Const n -> Const 0
	| Var str -> if str = x then Const 1 else Const 0
	| Power (str, num) ->
		if str = x
		then
			if num = 1 then   Const num
			else if num = 0 then Const 0
			else Times[Const num;Power(str, num-1)]
		else Const 0
	| Sum ls -> match ls with
		|[a] -> diff(a,x)
		|hd::tl -> Sum [diff(hd,x);diff(Sum tl,x)]
	|Times ls ->if List.mem (Const 0) ls then Const 0
              else (match ls with
                  | [a] ->diff(a,x);
                  | hd::tl ->match hd with
                            | Const a -> Times[hd;diff(Times tl,x)]
                            | _ ->Sum [Times [diff(hd,x);Times tl]; Times[hd;diff(Times tl,x)]])