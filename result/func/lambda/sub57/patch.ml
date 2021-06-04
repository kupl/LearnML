type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec comblist ((l1 : 'a list), (l2 : 'a list)) : 'a list =
  match l1 with [] -> l2 | hd :: tl -> hd :: comblist (tl, l2)


let rec searchlist ((a : string), (l : string list)) : bool =
  match l with
  | [] -> false
  | hd :: tl -> if hd = a then true else searchlist (a, tl)


let rec complist (l : string list) : string list =
  match l with
  | [] -> []
  | hd :: tl -> if searchlist (hd, tl) then complist tl else hd :: tl


let rec __s4 ((__s11 : string list), (__s12 : string)) : string list =
  match __s11 with
  | __s13 :: __s14 ->
      if __s13 = __s12 then __s4 (__s14, __s12) else __s13 :: __s4 (__s14, __s12)
  | [] -> []


let rec vars (e : lambda) : string list =
  match e with
  | V x -> [ x ]
  | P (x, e1) -> __s4 (vars e1, x)
  | C (e1, e2) -> comblist (vars e1, vars e2)


let rec used (e : lambda) : string list =
  match e with
  | V x -> []
  | P (x, e1) -> used e1
  | C (e1, e2) -> comblist (used e1, used e2)


let rec matchvar ((vr : string list), (us : string list)) : bool =
  match (vr, us) with
  | [], _ -> true
  | hd :: tl, us -> if searchlist (hd, us) then matchvar (tl, us) else false


let check (e : lambda) : bool = matchvar (complist (vars e), used e)
