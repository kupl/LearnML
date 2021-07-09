type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (e : lambda) : bool = checking (e, makelist (e, []))

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
