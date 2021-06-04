type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 ((__s4 : string), (__s5 : string list)) : bool =
  match (__s4, __s5) with
  | _, [] -> false
  | __s14, __s15 :: __s16 -> if __s14 = __s15 then true else __s3 (__s14, __s16)


let rec __s6 ((__s7 : lambda), (__s8 : string list)) : bool =
  match __s7 with
  | V __s9 -> __s3 (__s9, __s8)
  | P (__s10, __s11) -> __s6 (__s11, __s10 :: __s8)
  | C (__s12, __s13) -> __s6 (__s12, __s8) && __s6 (__s13, __s8)


let rec check (e : lambda) : bool = __s6 (e, [])

and makelist ((e : lambda), (l : string list)) : var list =
  match (e, l) with
  | V v, l -> l
  | P (v, e), l -> v :: makelist (e, l)
  | C (e1, e2), l1 ->
      let l2 : string list = makelist (e1, l1) in
      makelist (e2, l2)


and checking ((e : lambda), (l : string list)) : bool =
  match (e, l) with
  | V v, l -> findv (v, l)
  | P (v, e), l -> checking (e, l)
  | C (e1, e2), l -> checking (e1, l) && checking (e2, l)


and findv ((v : string), (l : string list)) : bool =
  match (v, l) with
  | _, [] -> false
  | v, hd :: tl -> if hd = v then true else findv (v, tl)
