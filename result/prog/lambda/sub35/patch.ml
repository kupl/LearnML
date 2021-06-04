type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec dec (met : lambda) (lst : string list) : bool =
  match met with
  | V a -> if List.mem a lst = true then true else false
  | C (m1, m2) -> dec m1 lst && dec m1 lst
  | P (n1, m1) -> dec m1 (n1 :: lst)


let rec __s1 (__s2 : lambda) (__s3 : var list) : bool =
  match __s2 with
  | V __s11 -> List.mem __s11 __s3
  | P (__s12, __s13) -> __s1 __s13 (__s3 @ [ __s12 ])
  | C (__s14, __s15) -> __s1 __s14 __s3 && __s1 __s15 __s3


let rec check (m : lambda) : bool =
  match m with
  | V a -> false
  | P (n1, m1) -> __s1 m1 [ n1 ]
  | C (m1, m2) -> dec m1 [] && dec m2 []
