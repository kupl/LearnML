
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check : lambda -> bool
  = fun lambda ->
    let rec ch :  lambda * var list -> bool
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
  in ch(lambda, [])
