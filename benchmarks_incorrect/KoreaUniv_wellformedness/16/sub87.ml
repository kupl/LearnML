
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec str exp
=match exp with
|V a -> []
|P (a, e1) -> a::(str e1 )
|C (e1,e2) -> (str e1)@(str e2)

let rec v_str exp
=match exp with
|V a -> [a]
|P (a, e1) -> (v_str e1)
|C (e1, e2) -> (v_str e1)@(v_str e2)

let rec search var exp
=match exp with
|[] -> false
|hd::tl -> if hd = var then true else search var tl

let rec compare e1 e2
=match e2 with
|[] -> true
|hd::tl -> if (search hd e1)&&(compare e1 tl) then true else false




let check : exp -> bool
  = fun exp -> 
compare (str exp) (v_str exp)
