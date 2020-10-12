
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let check : lambda -> bool
  = fun lambda -> 
    let rec deletehd : string -> string list -> bool 
    = fun var l -> 
    match l with
      | [] -> false
      | hd :: tl -> if hd = var then true 
                    else deletehd var tl
    in 

    let rec isbound : lambda -> string list -> bool
    = fun lambda lst ->
      match lambda with
        | P(var, lambda1) -> isbound lambda1 (var :: lst) 
        | C(lambda1, lambda2) -> isbound lambda1 lst && isbound lambda2 lst
        | V(var) -> deletehd var lst
    in 

    isbound lambda []
