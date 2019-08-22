type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool = 
  let rec checkr : lambda -> (var list) -> bool = 
    fun code vars ->
      match code with
      | V var -> (List.mem var vars)
      | C (code1, code2) ->
          (checkr code1 vars) && (checkr code2 vars)
      | P (new_var, code_in) -> checkr code_in (new_var::vars)
  in
  fun code -> checkr code []
