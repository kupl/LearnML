
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec lambdaToVar : lambda -> var
  = fun lambda ->
      match lambda with
      |V var        -> var
      |P (var,ex)   -> lambdaToVar(ex)
      |C (ex1,ex2)  -> lambdaToVar(ex2)

  let rec check : lambda -> bool
  = fun lambda -> 
    let rec pcheck : lambda * (var list) -> bool
    = fun (alambda, vlist) -> 
      match alambda with
        V s -> (match vlist with
          [] -> false
          | h::t -> if s = h then true else pcheck (V s, t))
      | P (v,lambda) -> pcheck (lambda, v::vlist)
      | C (lambda1, lambda2) -> (pcheck (lambda1,vlist)) && (pcheck (lambda2,vlist))            
    in 
  pcheck (lambda,[]);;
