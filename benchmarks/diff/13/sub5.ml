type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let diff (f, v) =
  let rec deployEnv env o = 
    match env with
  | a::b -> 
      (
       match a with 
       |(x, c, p) -> 
	   if o = 0 && c = 0 then deployEnv b o
	   else if x = "const" && o = 1 && c = 1 then deployEnv b o
	   else if p = 0 then (CONST c)::(deployEnv b o)
	   else if c = 1 && p = 1 then (VAR x)::(deployEnv b o)
	   else if p = 1 then TIMES[CONST c; VAR x]::(deployEnv b o)
	   else if c = 1 then POWER(x, p)::(deployEnv b o)
	   else TIMES [CONST c; POWER(x, p)]::(deployEnv b o)
      )
  | [] -> [] in
  let rec updateEnv o e env  = 
    match env with
    | (hd::tl) ->
	(
	 match hd with
	 | (s, c, p) -> 
	     (
	      match e with
		(s2, c2, p2) ->
		  if o = 0 then
		    if s = s2 && p = p2 then (s, (c + c2), p)::tl
		    else hd::(updateEnv o e tl)
		  else
		    if s = s2  then (s, (c*c2), (p + p2))::tl
		    else hd::(updateEnv o e tl)
	     )
	)
    | [] -> e::[] in
  let rec doDiff (f, v) =
    match f with
    | CONST _ -> CONST 0
    | VAR x ->
	if x = v then CONST 1
	else CONST 0
    | POWER (x, p) ->
	if p = 0 then CONST 0
	else if x = v then TIMES ((CONST p)::POWER (x, p-1)::[])
	else CONST 0
    | TIMES x -> 
	(match x with
	| (a::[]) -> doDiff (a, v)
	| (a::b) -> 
	    let da = doDiff(a, v) in
	    let db = doDiff((TIMES b), v) in
	    (
	     match (a, db, b, da) with
	     | (CONST p, CONST q, (CONST r)::[], CONST s) -> CONST (p*q + r*s)
	     | (CONST p, CONST q, _, _) -> 
		 if da = CONST 0 || b = (CONST 0)::[] then CONST (p*q)
		 else SUM (CONST(p*q)::TIMES(da::b)::[])
	     | (_, _, (CONST r)::[], CONST s) -> 
		 if a = CONST 0 || db = CONST 0 then CONST (r*s)
		 else  SUM (TIMES(a::db::[])::CONST(r*s)::[])
	     | _ -> 
		 if a = CONST 0 || db = CONST 0 then TIMES(da::b)
		 else if b = (CONST 0)::[] || da = CONST 0 then TIMES(a::db::[])
		 else SUM(TIMES(a::db::[])::TIMES(da::b)::[])
	    )
	| [] -> CONST 0)
    | SUM x -> SUM(List.map (fun x -> doDiff(x, v)) x) in
  let rec prettyAE f env o = 
    match f with
    | SUM x -> 
	( 
	  match x with
	  | (CONST a)::z -> prettyAE (SUM z) (updateEnv 0 ("const", a, 0) env) 0
	  | (VAR a)::z -> prettyAE (SUM z) (updateEnv 0 (a, 1, 1) env) 0
	  | (POWER (a, b))::z -> prettyAE (SUM z) (updateEnv 0 (a, 1, b) env ) 0
	  | (SUM y)::z -> prettyAE (SUM (List.append y z)) env 0
	  | (TIMES y)::z -> 
	      (
	       let l = prettyAE (TIMES y) [] 1 in
	       match l with
	       | h::t -> 
		   if t = [] then List.append l (prettyAE (SUM z) env 0)
		   else List.append (TIMES l::[]) (prettyAE (SUM z) env 0)
	       | [] -> []
	      )
	  | [] -> deployEnv env 0
	 )
    | TIMES x ->
	(
	 match x with
	 | (CONST a)::z -> prettyAE (TIMES z) (updateEnv 1 ("const", a, 0) env) 1
	 | (VAR a)::z -> prettyAE (TIMES z) (updateEnv 1 (a, 1, 1) env) 1
	 | (POWER (a, b))::z -> prettyAE (TIMES z) (updateEnv 1 (a, 1, b) env ) 1
	 | (SUM y)::z -> 
	     (
	      let l = prettyAE (SUM y) [] 0 in
	      match l with
	      | h::t -> 
		  if t = [] then List.append l (prettyAE (TIMES z) env 1)
		  else List.append (SUM l::[]) (prettyAE (TIMES z) env 1)
	      | [] -> []
	     )
	 | (TIMES y)::z ->  prettyAE (TIMES (List.append y z)) env 1
	 | [] -> deployEnv env 1
	) in


  let res = doDiff (f, v) in
  match res with
  | SUM _ -> SUM (prettyAE res [] 0)
  | TIMES _ -> TIMES (prettyAE res [] 1)
  | _ -> res
