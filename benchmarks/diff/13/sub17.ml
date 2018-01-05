exception InvalidArgument
exception L2AL_null
type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list
let rec diff(ae,str) = 
	let rec list2aelist(aelist,st)= 
		match aelist with
		|h::[] -> [diff(h,st)]
		|h::t -> [diff(h,st)]@(list2aelist(t,st))
		|[] -> raise L2AL_null
	in
	match ae with
	| CONST c -> CONST 0
	| VAR s
		 -> if(s=str) then CONST 1
		 	else CONST 0
	| POWER(s,n) ->
		 if(n==0) then CONST 1
		 else if(n==1&&s=str) then CONST 1
		 else if(n==1&&s!=str) then CONST 0
		 else if(s=str) then TIMES[CONST n;POWER(s,n-1)]
		 else CONST 0
	| SUM aelist ->(match aelist with
					|[] -> raise InvalidArgument
					|h::[] -> diff(h,str)
					|h::t -> SUM([diff(h,str)]@list2aelist(t,str)))
	| TIMES aelist ->(match aelist with
					|[] -> raise InvalidArgument
					|h::[] -> diff(h,str)
					|h::t -> SUM[TIMES([diff(h,str)]@t);TIMES([h]@[diff((TIMES t),str)])])
