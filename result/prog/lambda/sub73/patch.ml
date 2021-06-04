type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec comp (va : string) (exorg : lambda) : bool =
  match exorg with
  | V va1 -> false
  | P (va1, ex1) -> if va = va1 then true else comp va ex1
  | C (ex1, ex2) -> comp va ex1 || comp va ex2


let rec find (ex : lambda) (exorg : lambda) : bool =
  match ex with
  | V va1 -> comp va1 exorg
  | P (va1, ex1) -> find ex1 exorg
  | C (ex1, ex2) -> find ex1 exorg && find ex2 exorg


let rec __s1 (__s2 : lambda) (__s3 : string list) : bool =
  match __s2 with
  | V __s11 -> List.mem __s11 __s3
  | C (__s12, __s13) -> __s1 __s12 __s3 && __s1 __s13 __s3
  | P (__s14, __s15) -> __s1 __s15 (__s14 :: __s3)


let rec check (e : lambda) : bool =
  match e with
  | V va -> false
  | P (va, ex) -> __s1 ex [ va ]
  | C (ex1, ex2) -> check ex1 && check ex2
