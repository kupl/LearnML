type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

exception InvalidArgument

let diff (f, v) =
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
	(
	 match x with
	 | (a::[]) -> doDiff (a, v)
	 | (a::b) -> 
	     let da = doDiff(a, v) in
	     let db = doDiff((TIMES b), v) in
	     SUM(TIMES(a::db::[])::TIMES(da::b)::[])
	 | [] -> raise InvalidArgument
	)
    | SUM x -> 
	(
	 match x with 
	 | (a::[]) -> doDiff (a, v)
	 | (a::b) -> SUM(List.map (fun x -> doDiff(x, v)) x) 
	 | [] -> raise InvalidArgument
	) in
  doDiff (f, v)
  
