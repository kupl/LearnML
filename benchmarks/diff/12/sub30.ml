type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec diff (ae, var) =
    match ae with
    | CONST x -> CONST 0
    | VAR x -> CONST 1
    | POWER (x, i) -> 
        if i = 0 then CONST 0
        else if i = 1 then CONST 1
        else TIMES ((CONST i)::POWER(x, i-1)::[])
    | TIMES aelist ->
        (match aelist with
        | [] -> CONST 0
        | hd::tl -> SUM (TIMES (diff (hd, var)::tl)::(TIMES (hd::(diff (TIMES tl, var)::[])))::[])
		)
	| SUM aelist ->
		SUM (List.map (fun x -> diff (x, var)) aelist)
