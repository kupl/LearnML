
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec ch : lambda * lambda list -> bool
  = fun (lambda,lst) -> match lambda with
  | V (a) ->( match lst with
        | [] -> false
        | hd::tl -> if hd = V a then true
                else if tl = [] then false
                else ch (V a, tl)
  )
  | P (a,b) -> ch (b, V a::lst)
  | C (a,b) -> ch (a, lst) && ch (b, lst)

  let check : lambda -> bool
  = fun lambda -> ch (lambda , [])
