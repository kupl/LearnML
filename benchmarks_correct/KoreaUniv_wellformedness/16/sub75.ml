
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec lcheck : lambda * string list -> bool
  = fun (lambda,lst) ->
  match lambda with
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

  let check : lambda -> bool
  = fun lambda -> (* TODO *)
  lcheck (lambda , [])
