  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec pcheck : var list * exp -> bool
    =fun (var, exp) ->
      match exp with
          V evar -> List.exists(fun v -> v = evar) var
        | P (v, exp) -> pcheck(v::var, exp)
        | C (e1, e2) -> pcheck(var, e1) && pcheck(var ,e2)

  let check : exp -> bool
    =fun e ->
      match e with
          V v-> false
        | P (v, exp) -> pcheck(v::[], exp)
        | C (e1, e2) -> false
