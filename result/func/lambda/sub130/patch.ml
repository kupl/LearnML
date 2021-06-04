type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec doesExist ((var : string), (env : string list)) : bool =
  match env with
  | [] -> false
  | hd :: tl -> if var = hd then true else doesExist (var, tl)


let rec isBound ((lam : lambda), (env : string list)) : bool =
  match lam with
  | V v -> if doesExist (v, env) then true else false
  | P (v, l) -> isBound (l, env @ [ v ])
  | C (l1, l2) -> isBound (l1, env) && isBound (l2, env)


let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let rec check (lam : lambda) : bool = List.for_all isBound (__s3 (lam, []))
