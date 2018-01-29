
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec allvar: exp -> string list
  = fun exp ->
 match exp with
 | V v -> [v]
 | P (v, ex) -> []
 | C (ex1, ex2) -> allvar ex1 @ allvar ex2;;

let rec searchlist: (var list * var) -> bool
  = fun (list, var) ->
  match list with
  | [] -> false
  | hd::tl -> if (hd = var) then true
              else searchlist (tl, var);;

let rec complist: (var list * var list) -> bool
  = fun (l1, l2) ->
  match l2 with
  | [] -> true
  | hd::tl -> if searchlist (l1, hd) then complist (l1, tl)
              else false;;

let rec checktest: (exp * var list) -> bool
  = fun (exp, l) ->
  match exp with
  | V v -> true
  | P (v, ex) -> if complist ([v] @ l, allvar ex) then checktest (ex, [v] @ l)
                 else false
  | C (ex1, ex2) -> checktest (ex1, l) && checktest (ex2, l)

  let check : exp -> bool
  = fun exp ->
  match exp with 
  | V v -> false
  |_ -> checktest (exp, []);; (* TODO *)
