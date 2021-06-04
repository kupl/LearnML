type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lambda : lambda) : bool =
  if cklist lambda = [] then true else false


and cklist (lambda : lambda) : string list =
  match lambda with
  | V a -> [ a ]
  | P (a, e) -> List.filter (fun (__s8 : string) -> __s8 != a) (cklist e)
  | C (e1, e2) -> cklist e1 @ cklist e2


and mtlist ((a : string), (l : string list)) : bool =
  match l with
  | [] -> false
  | hd :: tl -> if hd = a then true else mtlist (a, tl)


and remlist ((a : string), (l : string list)) : string list =
  match l with
  | [] -> []
  | hd :: tl -> if hd = a then remlist (a, tl) else [ a ] @ remlist (a, tl)
