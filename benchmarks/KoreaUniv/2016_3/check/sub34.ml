
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp ->
    let rec ch :  exp * var list -> bool
    = fun (r, lst) ->
    match r with
    | V a ->
    begin
    match lst with
    | [] -> false
    | m::n -> if m = a then true else ch(r, n)
    end
    | P(a, b) -> ch(b, lst@[a])
    | C(a, b) -> ch(a, lst) && ch(b, lst)
  in ch(exp, [])
