
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec searchlist : var*var list -> bool
=fun(v,lst)->
match lst with
|hd::tl -> if hd=v then true else searchlist(v,tl)
|[]-> false

let rec search : exp*var list->bool
=fun(exp,lst)->
match exp with
|P(a,b)-> search(b,a::lst)
|V(a)->searchlist(a,lst)
|C(a,b)->if search(b,lst) && search(a,lst) then true
        else false

  let rec check : exp -> bool
  = fun exp ->
let lst = [] in
match exp with
|P(a,b) -> search(b,a::lst)
|V(a) -> false
|C(a,b) -> if check(a)=true && check(b)=true then true else false
