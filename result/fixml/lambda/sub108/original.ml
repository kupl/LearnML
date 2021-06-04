type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec getBoundValues : lambda -> var list =
 fun lambda ->
  match lambda with
  | V v -> []
  | P (v, e) -> v :: getBoundValues e
  | C (e1, e2) -> getBoundValues e1 @ getBoundValues e2


let rec getValues : lambda -> var list =
 fun lambda ->
  match lambda with
  | V v -> [ v ]
  | P (v, e) -> getValues e
  | C (e1, e2) -> getValues e1 @ getValues e2


let rec compareHelper : var * var list -> bool =
 fun (var, varL) ->
  match (var, varL) with
  | v, hd :: tl ->
      if v = hd then true else if tl = [] then false else compareHelper (v, tl)


let rec compare : var list * var list -> bool =
 fun (values, bound) ->
  match (values, bound) with
  | valueHd :: valueTl, bound ->
      if valueTl = [] then compareHelper (valueHd, bound)
      else if compareHelper (valueHd, bound) then compare (valueTl, bound)
      else false


let check : lambda -> bool =
 fun lambda -> compare (getValues lambda, getBoundValues lambda)
