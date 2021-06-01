
  type lambda = V of var
            |P of var * lambda
            |C of lambda * lambda
  and var = string

  let rec lcheck: lambda * var list -> bool
     = fun (lambda, vlist) ->
     match lambda with
       | V(var1) -> (match vlist with
                     | [] -> false
                     | hd :: tl -> if hd = var1 then true else lcheck(lambda, tl))
       | P(var1, lambda1) -> lcheck(lambda1,  var1 :: vlist)
       | C(lambda1, lambda2) -> lcheck(lambda1, vlist) && lcheck(lambda2, vlist)

  let check : lambda -> bool
     = fun lambda -> lcheck(lambda, [])
