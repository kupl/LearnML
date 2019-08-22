
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec chkList
    = fun (e,li) ->
        match e with
        | V var -> (match li with 
                      [] -> false
                      | h::t -> if (var = h) then true else chkList (V var, t) )
        | P (var,lambda) -> chkList (lambda, var::li)
        | C (lambda1, lambda2) -> if(chkList (lambda1,li) && chkList (lambda2,li)) then true else false
  let check : lambda -> bool
  = fun lambda -> chkList (lambda, [])
  