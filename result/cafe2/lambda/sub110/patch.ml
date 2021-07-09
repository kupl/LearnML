type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec allvar (lambda : lambda) : string list =
  match lambda with
  | V v -> [ v ]
  | P (v, ex) -> []
  | C (ex1, ex2) -> allvar ex1 @ allvar ex2


let rec searchlist ((lst : string list), (var : string)) : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if hd = var then true else searchlist (tl, var)


let rec complist ((l1 : string list), (l2 : string list)) : bool =
  match l2 with
  | [] -> true
  | hd :: tl -> if searchlist (l1, hd) then complist (l1, tl) else false


let rec checktest ((lambda : lambda), (l : string list)) : bool =
  match lambda with
  | V v -> (
      match l with
      | [] -> false
      | __s8 :: __s9 -> if v = v then true else checktest (lambda, __s9) )
  | P (v, ex) ->
      if complist ([ v ] @ l, allvar ex) then checktest (ex, [ v ] @ l)
      else false
  | C (ex1, ex2) -> checktest (ex1, l) && checktest (ex2, l)


let rec check (lambda : lambda) : bool =
  match lambda with V v -> false | _ -> checktest (lambda, [])
