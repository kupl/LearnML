  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> checking (e, makelist (e,[]))

  and makelist : exp * var list -> var list
  = fun (e, l) -> match (e,l) with
  | ((V v), l) -> l
  | ((P (v,e)), l)-> v::(makelist (e,l))
  | ((C (e1,e2)), l1) -> let l2 = makelist (e1, l1) in makelist (e2, l2)

  and checking : (exp * var list) -> bool
  = fun (e,l) -> match ( e, l) with
  | (V v, l) -> findv (v ,l)
  | (P (v,e) , l) -> checking (e,l)
  | (C (e1,e2) , l) -> (checking (e1, l) && checking (e2,l))

  and findv : var * var list -> bool
  = fun (v, l) -> match (v, l) with
  |(_,[]) -> false
  |(v, hd::tl )-> if hd = v then true else findv (v, tl)

