
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

   let rec strbool : string * string list -> bool
   = fun(var, strlist) ->
   match strlist with
   | [] -> false
   | hd::tl -> if hd = var then true else strbool(var, tl)

   let rec expbool : exp * string list -> bool
   = fun(exp, strlist) ->
   match exp with
   | V(var) -> strbool(var, strlist)
   | P(var, exp) -> expbool(exp, var::strlist)
   (*| P(var, exp) -> expbool(exp, [var::strlist])*)
   | C(e1, e2) -> if (expbool(e1, strlist) = true) && (expbool(e2, strlist) = true) then true else false

  let rec check : exp -> bool
  = fun exp -> (* raise NotImplemented  TODO *)
   match exp with
   | V(var) -> false
   | P(var, exp) -> expbool(exp, [var])
   (*| P(var, exp) -> expbool(exp, var)*)
   | C(e1, e2) -> if ((expbool(e1, []) && expbool(e2, [])) = true) then true else false
   (*| C(e1, e2) -> if ((expbool(e1, []) && (expbool(e2, [])) = true then true else false
 | C(e1, e2) -> if (expbool(e1, []) = true) && (expbool(e2, []) = true) then true else false*)
