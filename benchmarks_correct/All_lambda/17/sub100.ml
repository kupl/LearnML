type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec solver (ln,mt) : bool =
  match mt with
  | V(n) -> List.mem n ln
  | P(n,m) -> solver(ln@[n],m)
  | C(m1,m2) -> solver(ln,m1)&&solver(ln,m2)
let check mt : bool =
  solver([],mt)
