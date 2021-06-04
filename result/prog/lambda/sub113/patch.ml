type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec exist v (lst : 'a list) : bool =
  match lst with [] -> false | hd :: tl -> if v = hd then true else exist v tl


let rec checker (e : lambda) (lst : string list) : bool =
  match e with
  | V v -> exist v lst
  | P (v, e) -> checker e (lst @ [ v ])
  | C (e1, e2) -> if checker e1 lst = checker e2 lst then true else false


let rec __s3 (__s4 : lambda) (__s5 : var list) : bool =
  match __s4 with
  | V __s6 -> List.mem __s6 __s5
  | P (__s7, __s8) ->
      let __s9 : var list = __s7 :: __s5 in
      __s3 __s8 __s9
  | C (__s10, __s11) -> __s3 __s10 __s5 && __s3 __s11 __s5


let rec check (lambda : lambda) : bool = __s3 lambda []
