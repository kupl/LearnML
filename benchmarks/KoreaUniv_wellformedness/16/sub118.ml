
  type exp = V of var
            |P of var * exp
            |C of exp * exp
  and var = string

  let rec lcheck: exp * var list -> bool
     = fun (exp, vlist) ->
     match exp with
       | V(var1) -> (match vlist with
                     | [] -> false
                     | hd :: tl -> if hd = var1 then true else lcheck(exp, tl))
       | P(var1, exp1) -> lcheck(exp1,  var1 :: vlist)
       | C(exp1, exp2) -> lcheck(exp1, vlist) && lcheck(exp2, vlist)

  let check : exp -> bool
     = fun exp -> lcheck(exp, [])
