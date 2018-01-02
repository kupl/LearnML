type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

let isinthelist l a = 
let temp = List.find_all ((fun x y -> if x <> y then false else true) a) l in
match temp with
| [] -> false
| hd::tl -> true

let isnotinthelist l a = 
let temp = List.find_all ((fun x y -> if x <> y then false else true) a) l in
match temp with
| [] -> true
| hd::tl -> false

let rec pvar e =
match e with
| P (s0, e0) -> s0::(pvar e0)
| C (e0, e1) -> (pvar e0) @ (pvar e1)
| V s0 -> []

let rec vvar e =
match e with
| P (s0, e0) -> vvar e0
| C (e0, e1) -> (vvar e0) @ (vvar e1)
| V s0 -> s0::[]

let rec check e =
let l0 = pvar e in
let l1 = vvar e in
match List.filter (isnotinthelist l0) l1 with
| [] -> true
| hd::tl -> false