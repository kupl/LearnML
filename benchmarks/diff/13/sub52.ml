type ae = CONST of int
  |VAR of string
  |POWER of string * int
  |TIMES of ae list
  |SUM of ae list
exception InvalidArgument
let rec diff (ae,dx) =
  match ae with
  |CONST a -> CONST 0
  |VAR a -> if a=dx then CONST 1 else CONST 0
  |POWER (a,b) ->
      if a=dx then 
	(if b=0 then CONST 0
	else TIMES [CONST b;POWER (a,b-1)])
	else CONST 0
  |TIMES list -> (match list with
    |[] -> raise InvalidArgument
    |[one] -> diff (one,dx)
    |_ -> SUM [TIMES ((diff (List.hd list, dx))::List.tl list); TIMES ([(List.hd list);(diff (TIMES (List.tl list),dx))])])
  |SUM list -> if []=list then raise InvalidArgument
  else SUM (List.map (fun a -> diff (a,dx)) list)
