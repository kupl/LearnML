
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec ch : exp * exp list -> bool
  = fun (exp,list) -> match exp with
  | V (a) ->( match list with
        | [] -> false
        | hd::tl -> if hd = V a then true
                else if tl = [] then false
                else ch (V a, tl)
  )
  | P (a,b) -> ch (b, V a::list)
  | C (a,b) -> ch (a, list) && ch (b, list)

  let check : exp -> bool
  = fun exp -> ch (exp , [])
