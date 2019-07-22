type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

exception InvalidArgument

let rec diff (aexpr, x) =
  match aexpr with
    CONST y -> (CONST 0)
  | VAR y -> if x=y then (CONST 1) else (CONST 0)
  | POWER(y, n) -> if (x=y) then (TIMES [(CONST n); (POWER(y, n-1))])
  				   else (CONST 0)
  | TIMES l -> let len = (List.length l) in
				 if len=0 then raise InvalidArgument
  			     else let hd = (List.hd l) in
					  let tl = (List.tl l) in
 			   	 		if len=1 then (diff (hd, x))
						else (SUM [(TIMES (List.append [(diff ((TIMES [hd]), x))] tl)); (TIMES [hd; (diff ((TIMES tl), x))])])
  | SUM l -> let len = (List.length l) in
  			   if len=0 then raise InvalidArgument
  			   else let hd = (List.hd l) in
					let tl = (List.tl l) in
  					  if len=1 then (diff (hd, x))
					  else (SUM [(diff (hd, x)); (diff ((SUM tl), x))])
