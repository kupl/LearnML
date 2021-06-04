type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec getBoundValues (lambda : lambda) : var list =
  match lambda with
  | V v -> []
  | P (v, e) -> v :: getBoundValues e
  | C (e1, e2) -> getBoundValues e1 @ getBoundValues e2


let rec getValues (lambda : lambda) : var list =
  match lambda with
  | V v -> [ v ]
  | P (v, e) -> getValues e
  | C (e1, e2) -> getValues e1 @ getValues e2


let rec compareHelper ((var : string), (varL : string list)) : bool =
  match (var, varL) with
  | v, hd :: tl ->
      if v = hd then true else if tl = [] then false else compareHelper (v, tl)


let rec compare ((values : string list), (bound : string list)) : bool =
  match (values, bound) with
  | valueHd :: valueTl, bound ->
      if valueTl = [] then compareHelper (valueHd, bound)
      else if compareHelper (valueHd, bound) then compare (valueTl, bound)
      else false


let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let __s6 (__s7 : lambda * string list) : bool =
  match __s7 with
  | V __s8, __s9 -> List.mem __s8 __s9
  | P (__s10, __s11), __s12 -> false
  | C (__s13, __s14), __s15 -> false


let check (lambda : lambda) : bool = List.for_all __s6 (__s3 (lambda, []))
