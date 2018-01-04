type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let rec diff (ex, str) =

	let rec ldiff sl el =
                match sl with
		| [] -> el
                | a::l1 -> (ldiff l1 (el@[diff(a,str)]))
        in

	let rec timesdiff l sl el =
		match l with
		| [] -> el
	        | a::l1 -> (timesdiff l1 (sl@[a]) (el@[TIMES (sl@[diff(a,str)]@l1)]))
        in

		
	 (* 미분해야되면=const가 살아있으면 true
           0이 되야 되면 false *)
        	
	let rec constcheck (e1, st)=
        match e1 with
        | (CONST a)::b -> constcheck (b, st)
        | (VAR a)::b -> if a=st then true else false
        | (POWER (a,n))::b -> if a=st then true else false
        | (TIMES a)::b -> if constcheck(a,st)=true then constcheck (b, st) else false
        | (SUM a)::b -> if constcheck(a,st)=true then constcheck (b, st) else false
        | [] -> false

        in
(*
	let rec varcheck (e1, st, sum)=
        match e1 with
        | (CONST a)::b -> varcheck (b, st, sum)
        | (VAR a)::b -> if a=st then varcheck (b, st, sum+1) else varcheck (b, st, sum)
        | (POWER (a,n))::b -> if a=st then varcheck (b, st, sum+n) else varcheck (b, st, sum)
        | (TIMES a)::b -> varcheck(b,st,sum+varcheck(TIMES a,st,0))
        | (SUM a)::b -> if constcheck(a,st)=true then constcheck (b, st) else false
        | [] -> sum

        in
*)
	let rec mibun (e1, st) =
	match e1 with
	| CONST n -> CONST 0
	| VAR s -> if st=s then CONST 1 else CONST 0
	| POWER (s,n) -> if st=s then TIMES [CONST n;POWER (s,(n-1))]
			else CONST 0
	| TIMES l -> SUM (timesdiff l [] [])
	| SUM l -> SUM (ldiff l [])
	
	in
	

	mibun(ex, str)

;;
