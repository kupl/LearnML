type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check met=

let rec checkArea id m =
match m with
V n -> List.exists (fun x->x=n) id
| C (s1,s2) -> ((checkArea id s1) & (checkArea id s2))
| P (id2,m) -> checkArea (id2::id) m
in

match met with
P (id,m) -> checkArea [id] m
| _ -> false