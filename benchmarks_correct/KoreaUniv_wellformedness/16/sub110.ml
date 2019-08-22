
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

   let rec strbool : string * string list -> bool
   = fun(var, strlist) ->
   match strlist with
   | [] -> false
   | hd::tl -> if hd = var then true else strbool(var, tl)

   let rec lambdabool : lambda * string list -> bool
   = fun(lambda, strlist) ->
   match lambda with
   | V(var) -> strbool(var, strlist)
   | P(var, lambda) -> lambdabool(lambda, var::strlist)
   (*| P(var, lambda) -> lambdabool(lambda, [var::strlist])*)
   | C(e1, e2) -> if (lambdabool(e1, strlist) = true) && (lambdabool(e2, strlist) = true) then true else false

  let rec check : lambda -> bool
  = fun lambda -> (* raise NotImplemented  TODO *)
   match lambda with
   | V(var) -> false
   | P(var, lambda) -> lambdabool(lambda, [var])
   (*| P(var, lambda) -> lambdabool(lambda, var)*)
   | C(e1, e2) -> if ((lambdabool(e1, []) && lambdabool(e2, [])) = true) then true else false
   (*| C(e1, e2) -> if ((lambdabool(e1, []) && (lambdabool(e2, [])) = true then true else false
 | C(e1, e2) -> if (lambdabool(e1, []) = true) && (lambdabool(e2, []) = true) then true else false*)
