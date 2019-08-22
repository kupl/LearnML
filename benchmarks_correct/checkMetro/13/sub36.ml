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
    let rec check = fun (lambda, environ) ->
        match lambda with
        | V var -> contains(environ, var)
        | P (var, lambda) -> check(lambda, var::environ)
        | C (m1, m2) -> check(m1, environ) && check(m2, environ)
    in
    check(lambda, [])

