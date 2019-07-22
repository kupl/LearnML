type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument

let rec diff (aein,str) =
 match aein with
 | CONST n -> CONST 0
 | VAR s -> if s=str then CONST 1 else CONST 0
 | POWER(s,n) -> if s=str then
                   if n=0 then CONST 0
                   else if n=1 then CONST 1
                   else TIMES((CONST n)::POWER (s,n-1)::[])
                  else CONST 0
 | TIMES([]) -> raise InvalidArgument
 | SUM([]) -> raise InvalidArgument
 | TIMES(hd::tl) -> if tl=[] then diff(hd,str)
                    else SUM( (TIMES( (diff(hd,str))::tl ))::(TIMES( hd::(diff(TIMES(tl),str))::[] ))::[] )
 | SUM(hd::tl) -> if tl=[] then diff(hd,str)
                  else SUM( (diff(hd,str))::(diff(SUM tl,str))::[] )
