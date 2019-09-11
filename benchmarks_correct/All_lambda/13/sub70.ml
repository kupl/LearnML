type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec licheck s1 lst1 =
  match lst1 with
  | head::tail -> (s1 = head) || (licheck s1 tail)
  | [] -> false

let rec sicheck m lst =
  match m with
  | P(s,m1) -> (sicheck m1 (s::lst))
  | C(m1,m2) -> (sicheck m1 lst) && (sicheck m2 lst)
  | V s -> (licheck s lst)

let rec check mtr =
 sicheck mtr []