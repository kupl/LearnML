type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s1 ((__s4 : string list), (__s5 : string)) : var list =
  match __s4 with
  | [] -> []
  | __s6 :: __s7 ->
      if __s6 = __s5 then __s1 (__s7, __s5) else [ __s6 ] @ __s1 (__s7, __s5)


let rec __s2 ((__s8 : string list), (__s9 : string)) : var list =
  match __s8 with
  | [] -> [ __s9 ]
  | __s10 :: __s11 ->
      if __s10 = __s9 then __s8 else [ __s10 ] @ __s2 (__s11, __s9)


let rec __s3 ((__s12 : lambda), (__s13 : string list)) : var list =
  match __s12 with
  | V __s14 -> __s2 (__s13, __s14)
  | P (__s15, __s16) -> __s1 (__s3 (__s16, __s13), __s15)
  | C (__s17, __s18) -> __s3 (__s17, __s13) @ __s3 (__s18, __s13)


let rec check (e : lambda) : bool =
  match __s3 (e, []) with [] -> true | _ -> false


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
