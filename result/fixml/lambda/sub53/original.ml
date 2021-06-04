type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec matchid ((x : string), (l : string list)) : bool =
  match l with
  | [] -> false
  | hd :: tl -> if hd = x then true else matchid (x, tl)


let rec ckMetro ((m : lambda), (idl : string list)) : bool =
  match m with
  | V x -> matchid (x, idl)
  | P (id, V x) -> ckMetro (V x, id :: idl)
  | P (id, some) -> ckMetro (some, id :: idl)
  | C (V x, V y) -> matchid (x, idl) && matchid (y, idl)
  | C (V x, some) -> if matchid (x, idl) then ckMetro (some, idl) else false
  | C (x, y) -> ckMetro (x, idl) && ckMetro (y, [])


let rec check (m : lambda) : bool = ckMetro (m, [])
