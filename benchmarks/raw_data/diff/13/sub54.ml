type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument

let rec diff (exp, diffwith) =
	match exp with
	| Const(num) -> Const 0
	| Var(var) -> if var=diffwith then Const 1 else Const 0
	| Power(var, num) -> 
		if var=diffwith && num=0 then Const 0 
		else if var=diffwith && num!=0 then Times ((Const num)::(Power(var, num-1))::[])
		else Const 0 
	| Times(aexplist) ->
		if (List.length aexplist)>1 then
			Sum( (Times (List.append ((diff ((List.hd aexplist), diffwith))::[]) (List.tl aexplist)))::(Times ((List.hd aexplist)::(diff ((Times (List.tl aexplist)), diffwith))::[]))::[])
		else if (List.length aexplist)=0 then raise InvalidArgument
		else (diff ((List.hd aexplist), diffwith))
	| Sum(aexplist) ->
		if (List.length aexplist)>1 then
			Sum((diff ((List.hd aexplist), diffwith))::(diff ((Sum (List.tl aexplist)), diffwith))::[])
		else if (List.length aexplist)=0 then raise InvalidArgument
		else (diff ((List.hd aexplist), diffwith))
