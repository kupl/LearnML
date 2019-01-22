
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec chkList
    = fun (e,li) ->
        match e with
        | V var -> (match li with 
                      [] -> false
                      | h::t -> if (var = h) then true else chkList (V var, t) )
        | P (var,exp) -> chkList (exp, var::li)
        | C (exp1, exp2) -> if(chkList (exp1,li) && chkList (exp2,li)) then true else false
  let check : exp -> bool
  = fun exp -> chkList (exp, [])
  