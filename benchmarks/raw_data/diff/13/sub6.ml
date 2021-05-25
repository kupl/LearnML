type aexp = Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let diff (f, v) =
  let rec doDiff (f, v) =
    match f with
    | Const _ -> Const 0
    | Var x ->
	if x = v then Const 1
	else Const 0
    | Power (x, p) ->
	if p = 0 then Const 0
	else if x = v then Times ((Const p)::Power (x, p-1)::[])
	else Const 0
    | Times x -> 
	(
	 match x with
	 | (a::[]) -> doDiff (a, v)
	 | (a::b) -> 
	     let da = doDiff(a, v) in
	     let db = doDiff((Times b), v) in
	     Sum(Times(a::db::[])::Times(da::b)::[])
	 | [] -> raise InvalidArgument
	)
    | Sum x -> 
	(
	 match x with 
	 | (a::[]) -> doDiff (a, v)
	 | (a::b) -> Sum(List.map (fun x -> doDiff(x, v)) x) 
	 | [] -> raise InvalidArgument
	) in
  doDiff (f, v)
  
