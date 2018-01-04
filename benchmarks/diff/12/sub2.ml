type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let rec diff: ae*string->ae = fun( alexp, str )->
	let zeroTIMES: ae->ae = fun a->
		match a with
		TIMES( hd::tl )->
			if hd = CONST 0 || List.mem (CONST 0) tl then CONST 0
			else a
		|_->a
	in 

	match alexp with
	|CONST c-> CONST 0
	|VAR s->
		if String.compare s str !=0 then CONST 0
		else CONST 1	
	|POWER(s,n)->
		if String.compare s str !=0 then CONST 0
		else if n=1 then CONST n
		else if n=2 then TIMES((CONST n)::((VAR s)::[]))
		else if n>2 then TIMES((CONST n)::(POWER(s,n-1)::[]))
		else CONST 0
	|TIMES( hd::tl )->
		if tl=[] then diff(hd,str)
		else
			let ht = zeroTIMES (TIMES(diff(hd,str)::tl)) in
			let st = zeroTIMES (TIMES(hd::(diff(TIMES tl,str)::[]))) in
			if ht=CONST 0&&st=CONST 0 then CONST 0
			else if ht=CONST 0 then st
			else if st=CONST 0 then ht
			else SUM(ht::(st::[]))
	|TIMES [] -> CONST 0
	|SUM( hd::tl )->
		if tl=[] then diff(hd,str)
		else
			let h = diff(hd,str)in
			let t = diff(SUM tl,str)in
			if h=CONST 0 then t
			else if t= CONST 0 then h
			else SUM(h::(t::[]))
	|SUM [] -> CONST 0
