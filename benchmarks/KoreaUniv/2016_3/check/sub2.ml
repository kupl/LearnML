
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let check : exp -> bool
  = fun exp -> 
    let rec deletehd : string -> string list -> bool 
    = fun var l -> 
    match l with
      | [] -> false
      | hd :: tl -> if hd = var then true 
                    else deletehd var tl
    in 

    let rec isbound : exp -> string list -> bool
    = fun exp lst ->
      match exp with
        | P(var, exp1) -> isbound exp1 (var :: lst) 
        | C(exp1, exp2) -> isbound exp1 lst && isbound exp2 lst
        | V(var) -> deletehd var lst
    in 

    isbound exp []
