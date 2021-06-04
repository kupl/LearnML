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


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let check (lambda : lambda) : bool = List.length (__s3 lambda) = 0
