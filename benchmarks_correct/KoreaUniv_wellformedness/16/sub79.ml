
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec searchlist : var*var list -> bool
=fun(v,lst)->
match lst with
|hd::tl -> if hd=v then true else searchlist(v,tl)
|[]-> false

let rec search : lambda*var list->bool
=fun(lambda,lst)->
match lambda with
|P(a,b)-> search(b,a::lst)
|V(a)->searchlist(a,lst)
|C(a,b)->if search(b,lst) && search(a,lst) then true
        else false

  let rec check : lambda -> bool
  = fun lambda ->
let lst = [] in
match lambda with
|P(a,b) -> search(b,a::lst)
|V(a) -> false
|C(a,b) -> if check(a)=true && check(b)=true then true else false
