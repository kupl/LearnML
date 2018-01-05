type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument
let rec diff(a, stri) =
    match a with
    |CONST na -> CONST 0
    |VAR strin -> if stri = strin then CONST 1
                  else CONST 0
    |POWER (strin, na) -> if not(strin = stri) then CONST 0
						  else if na = 0 then CONST 0
						  else if na = 1 then CONST 1
						  else if na = 2 then TIMES[CONST 2; VAR strin]
						  else TIMES[CONST na; POWER (strin, na-1)]
    |TIMES (CONST na::tl) -> if na =0 then CONST 0
							else if na=1 then diff(TIMES tl, stri)
							else TIMES [CONST na;diff(TIMES tl, stri)]
	|TIMES (hd::tl)	-> if tl = [] then diff(hd, stri)
                       else SUM [TIMES (diff(hd, stri)::tl);TIMES [hd;diff(TIMES tl, stri)]]
	|TIMES [] -> raise InvalidArgument
	|SUM (CONST na :: tl) -> diff(SUM tl, stri)
	|SUM (hd::tl) -> if tl = [] then diff (hd, stri)
				   else SUM [diff(hd, stri);diff(SUM tl, stri)]
	|SUM [] -> raise InvalidArgument
