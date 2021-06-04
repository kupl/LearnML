type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec find : var list * var -> bool =
 fun (lst, v) ->
  match lst with
  | hd :: tl -> if hd = v then true else find (tl, v)
  | _ -> false


let rec f : var list * lambda -> bool =
 fun (lst, e) ->
  match e with
  | V v -> if find (lst, v) then true else false
  | P (v, e) -> if f (lst @ [ v ], e) then true else false
  | C (e1, e2) -> if check e1 && check e2 then true else false


and check : lambda -> bool =
 fun lambda ->
  match lambda with
  | V v -> false
  | P (v, e) -> if f ([ v ], e) then true else false
  | C (e1, e2) -> if check e1 && check e2 then true else false
