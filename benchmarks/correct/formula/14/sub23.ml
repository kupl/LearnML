(*컴퓨터공학부 2010-11779 박진영 1.2*)
exception Rec_exception

type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let eval fm =

let rec cal e =
match e with
| Num i -> e
| Plus (e1, e2) -> 
  (match (cal e1, cal e2) with
  | (Num i1, Num i2) -> Num (i1+i2)
  | _ -> raise Rec_exception
  ) 
| Minus (e1, e2) ->
  (match (cal e1, cal e2) with
  | (Num i1, Num i2) -> Num (i1-i2)
  | _ -> raise Rec_exception
  ) 
in

let rec evalfunc fm =
match fm with
| True -> True
| False -> False
| Not f -> 
  ( match evalfunc f with
  | True -> False
  | False -> True
  | _ -> raise Rec_exception
  )
| AndAlso (f1, f2) ->
  ( match (evalfunc f1, evalfunc f2) with
  | (True, True) -> True
  | _ -> False
  )
| OrElse (f1, f2) ->
  ( match (evalfunc f1, evalfunc f2) with
  | (False, False) -> False
  | _ -> True
  )
| Imply (f1, f2) ->
  ( match (evalfunc f1, evalfunc f2) with
  | (True, False) -> False
  | _ -> True
  )
| Equal (e1, e2) ->
  (match (cal e1, cal e2) with
  | (Num i1, Num i2) ->
    ( if i1 = i2 then True
      else False
    )
  | _ -> raise Rec_exception
  )
in

match evalfunc fm with
| True -> true
| False -> false
| _ -> raise Rec_exception
