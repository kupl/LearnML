type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff (ex, str) =

	let rec ldiff sl el =
                match sl with
		| [] -> el
                | a::l1 -> (ldiff l1 (el@[diff(a,str)]))
        in

	let rec timesdiff l sl el =
		match l with
		| [] -> el
	        | a::l1 -> (timesdiff l1 (sl@[a]) (el@[Times (sl@[diff(a,str)]@l1)]))
        in

		
        	
	let rec constcheck (e1, st)=
        match e1 with
        | (Const a)::b -> constcheck (b, st)
        | (Var a)::b -> if a=st then true else false
        | (Power (a,n))::b -> if a=st then true else false
        | (Times a)::b -> if constcheck(a,st)=true then constcheck (b, st) else false
        | (Sum a)::b -> if constcheck(a,st)=true then constcheck (b, st) else false
        | [] -> false

        in
(*
	let rec varcheck (e1, st, sum)=
        match e1 with
        | (Const a)::b -> varcheck (b, st, sum)
        | (Var a)::b -> if a=st then varcheck (b, st, sum+1) else varcheck (b, st, sum)
        | (Power (a,n))::b -> if a=st then varcheck (b, st, sum+n) else varcheck (b, st, sum)
        | (Times a)::b -> varcheck(b,st,sum+varcheck(Times a,st,0))
        | (Sum a)::b -> if constcheck(a,st)=true then constcheck (b, st) else false
        | [] -> sum

        in
*)
	let rec mibun (e1, st) =
	match e1 with
	| Const n -> Const 0
	| Var s -> if st=s then Const 1 else Const 0
	| Power (s,n) -> if st=s then Times [Const n;Power (s,(n-1))]
			else Const 0
	| Times l -> Sum (timesdiff l [] [])
	| Sum l -> Sum (ldiff l [])
	
	in
	

	mibun(ex, str)

;;
