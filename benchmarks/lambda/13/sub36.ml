(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check = fun lambda ->
    let rec contains = fun (lst, x) ->
        match lst with
        | [] -> false
        | h::t -> if h = x then true
                  else contains(t, x)
    in
    let rec check2 = fun (lambda, environ) ->
        match lambda with
        | V var -> contains(environ, var)
        | P (var, lambda) -> check2(lambda, var::environ)
        | C (m1, m2) -> check2(m1, environ) && check2(m2, environ)
    in
    check2(lambda, [])

