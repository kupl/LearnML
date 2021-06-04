type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check_list ((lambda : lambda), (lst : string list)) : bool =
  match lambda with
  | V var -> if List.mem var lst then true else false
  | P (var, lambda) -> check_list (lambda, lst @ [ var ])
  | C (lambda1, lambda2) ->
      check_list (lambda2, lst) && check_list (lambda2, lst)


let rec __s1 ((__s2 : lambda), (__s3 : var list)) : bool =
  match __s2 with
  | V __s6 -> List.mem __s6 __s3
  | P (__s7, __s8) -> __s1 (__s8, __s3 @ [ __s7 ])
  | C (__s9, __s10) -> __s1 (__s9, __s3) && __s1 (__s10, __s3)


let check (e : lambda) : bool = __s1 (e, [])
