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
  | V v -> true
  | P (v, ex) ->
      if complist ([ v ] @ l, allvar ex) then checktest (ex, [ v ] @ l)
      else false
  | C (ex1, ex2) -> checktest (ex1, l) && checktest (ex2, l)


let rec __s2 ((__s5 : lambda), (__s6 : string list)) : bool =
  match __s5 with
  | V __s7 -> (
      match __s6 with
      | [] -> false
      | __s8 :: __s9 -> if __s8 = __s7 then true else __s2 (__s5, __s9) )
  | P (__s10, __s11) -> __s2 (__s11, __s10 :: __s6)
  | C (__s12, __s13) ->
      if __s2 (__s12, __s6) = true then
        if __s2 (__s13, __s6) = true then true else false
      else false


let rec check (lambda : lambda) : bool =
  match lambda with V v -> false | _ -> __s2 (lambda, [])
