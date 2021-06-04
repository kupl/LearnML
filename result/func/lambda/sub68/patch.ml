type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let rec check (e : lambda) : bool = List.for_all checking (__s3 (e, []))

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
