
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec lcheck : exp * string list -> bool
  = fun (exp,lst) ->
  match exp with
  | V (a) ->(
    match lst with
    | [] -> false
    | hd::tl ->
      if hd = a then true
      else lcheck (V a, tl)
  )
  | P (a,b) ->
    lcheck (b, a::lst)
  | C (a,b) ->
    lcheck (a, lst) && lcheck (b, lst)

  let check : exp -> bool
  = fun exp -> (* TODO *)
  lcheck (exp , [])
