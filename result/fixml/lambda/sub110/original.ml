type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec allvar : lambda -> string list =
 fun lambda ->
  match lambda with
  | V v -> [ v ]
  | P (v, ex) -> []
  | C (ex1, ex2) -> allvar ex1 @ allvar ex2


let rec searchlist : var list * var -> bool =
 fun (lst, var) ->
  match lst with
  | [] -> false
  | hd :: tl -> if hd = var then true else searchlist (tl, var)


let rec complist : var list * var list -> bool =
 fun (l1, l2) ->
  match l2 with
  | [] -> true
  | hd :: tl -> if searchlist (l1, hd) then complist (l1, tl) else false


let rec checktest : lambda * var list -> bool =
 fun (lambda, l) ->
  match lambda with
  | V v -> true
  | P (v, ex) ->
      if complist ([ v ] @ l, allvar ex) then checktest (ex, [ v ] @ l)
      else false
  | C (ex1, ex2) -> checktest (ex1, l) && checktest (ex2, l)


let rec check : lambda -> bool =
 fun lambda -> match lambda with V v -> false | _ -> checktest (lambda, [])
