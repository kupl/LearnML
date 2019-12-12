type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument
let rec diff(a, stri) =
    match a with
    |Const na -> Const 0
    |Var strin -> if stri = strin then Const 1
                  else Const 0
    |Power (strin, na) -> if not(strin = stri) then Const 0
						  else if na = 0 then Const 0
						  else if na = 1 then Const 1
						  else if na = 2 then Times[Const 2; Var strin]
						  else Times[Const na; Power (strin, na-1)]
    |Times (Const na::tl) -> if na =0 then Const 0
							else if na=1 then diff(Times tl, stri)
							else Times [Const na;diff(Times tl, stri)]
	|Times (hd::tl)	-> if tl = [] then diff(hd, stri)
                       else Sum [Times (diff(hd, stri)::tl);Times [hd;diff(Times tl, stri)]]
	|Times [] -> raise InvalidArgument
	|Sum (Const na :: tl) -> diff(Sum tl, stri)
	|Sum (hd::tl) -> if tl = [] then diff (hd, stri)
				   else Sum [diff(hd, stri);diff(Sum tl, stri)]
	|Sum [] -> raise InvalidArgument
