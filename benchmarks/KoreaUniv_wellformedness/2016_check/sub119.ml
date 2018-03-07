
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec ch : exp * exp list -> bool
  = fun (exp,lst) -> match exp with
  | V (a) ->( match lst with
        | [] -> false
        | hd::tl -> if hd = V a then true
                else if tl = [] then false
                else ch (V a, tl)
  )
  | P (a,b) -> ch (b, V a::lst)
  | C (a,b) -> ch (a, lst) && ch (b, lst)

  let check : exp -> bool
  = fun exp -> ch (exp , [])
