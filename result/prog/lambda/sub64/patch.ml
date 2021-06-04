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


let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match __s4 with
  | V __s11 -> List.mem __s11 __s5
  | C (__s12, __s13) -> __s3 (__s12, __s5) && __s3 (__s13, __s5)
  | P (__s14, __s15) -> __s3 (__s15, List.append __s5 [ __s14 ])


let rec check (e : lambda) : bool =
  match e with
  | V v -> false
  | P (v, e') -> __s3 (e, [])
  | C (e1, e2) -> check e1 && check e2
