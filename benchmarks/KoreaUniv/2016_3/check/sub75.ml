
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec lcheck : exp * string list -> bool
  = fun (exp,list) ->
  match exp with
  | V (a) ->(
    match list with
    | [] -> false
    | hd::tl ->
      if hd = a then true
      else lcheck (V a, tl)
  )
  | P (a,b) ->
    lcheck (b, a::list)
  | C (a,b) ->
    lcheck (a, list) && lcheck (b, list)

  let check : exp -> bool
  = fun exp -> (* TODO *)
  lcheck (exp , [])
