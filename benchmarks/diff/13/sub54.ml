type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument

let rec diff (exp, diffwith) =
	match exp with
	| CONST(num) -> CONST 0
	| VAR(var) -> if var=diffwith then CONST 1 else CONST 0
	| POWER(var, num) -> 
		if var=diffwith && num=0 then CONST 0 
		else if var=diffwith && num!=0 then TIMES ((CONST num)::(POWER(var, num-1))::[])
		else CONST 0 
	| TIMES(aelist) ->
		if (List.length aelist)>1 then
			SUM( (TIMES (List.append ((diff ((List.hd aelist), diffwith))::[]) (List.tl aelist)))::(TIMES ((List.hd aelist)::(diff ((TIMES (List.tl aelist)), diffwith))::[]))::[])
		else if (List.length aelist)=0 then raise InvalidArgument
		else (diff ((List.hd aelist), diffwith))
	| SUM(aelist) ->
		if (List.length aelist)>1 then
			SUM((diff ((List.hd aelist), diffwith))::(diff ((SUM (List.tl aelist)), diffwith))::[])
		else if (List.length aelist)=0 then raise InvalidArgument
		else (diff ((List.hd aelist), diffwith))
