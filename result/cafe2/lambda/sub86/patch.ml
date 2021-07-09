type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec find ((lst : string list), (v : string)) : bool =
  match lst with
  | hd :: tl -> if hd = v then true else find (tl, v)
  | _ -> false


let rec f ((lst : string list), (e : lambda)) : bool =
  match e with
  | V v -> if find (lst, v) then true else false
  | P (v, e) -> if f (lst @ [ v ], e) then true else false
  | C (e1, e2) -> f (lst, e1) && f (lst, e2)


and check (lambda : lambda) : bool =
  match lambda with
  | V v -> false
  | P (v, e) -> if f ([ v ], e) then true else false
  | C (e1, e2) -> if check e1 && check e2 then true else false
