type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff(ae, string) =
	match ae with
	| CONST i -> (CONST 0)
	| VAR s -> 
		if (Pervasives.compare s string) == 0 then (CONST 1)
		else (CONST 0)
	| POWER (s, i) ->
		if (Pervasives.compare s string) == 0 then TIMES [(CONST i); (POWER(s, i - 1))]
		else (CONST 0)
	| TIMES aelist ->
		if List.length aelist < 1 then raise InvalidArgument
		else 
			let ret = ref [] in
			for i = 0 to List.length aelist - 1 do
				let temp = ref [] in
				for j = 0 to List.length aelist - 1 do
					if i == j
					then temp := diff(List.nth aelist i, string)::!temp
					else temp := (List.nth aelist j)::!temp
				done;
				temp := List.rev !temp;
				ret := (TIMES !temp)::!ret
			done;
			ret := List.rev !ret;
			(SUM !ret)
	| SUM aelist ->
		if List.length aelist < 1 then raise InvalidArgument
		else if List.length aelist == 2
			then SUM [(diff(List.nth aelist 0, string)); diff(List.nth aelist 1, string)]
		else
			SUM [(diff(List.hd aelist, string)); (diff((SUM (List.tl aelist)), string))]