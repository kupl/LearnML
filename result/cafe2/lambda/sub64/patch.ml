type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec change ((v : string), (e : lambda)) : lambda =
  match e with
  | V v' -> V v'
  | P (v', e') -> change (v', e')
  | C (e1, e2) ->
      if V v = e1 then if V v = e2 then change (v, e1) else change (v, e2)
      else if V v = e2 then change (v, e1)
      else change (v, V v)


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool = List.length (__s3 e) = 0
