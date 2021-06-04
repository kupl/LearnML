type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec evalCheck1 (ex : lambda) (bd : string) : bool =
  match ex with
  | V x -> false
  | P (arg, body) -> if arg = bd then true else evalCheck1 body bd
  | C (a, b) -> evalCheck1 a bd || evalCheck1 b bd


let rec evalCheck2 (ex : lambda) (bd : lambda) : bool =
  match bd with
  | V x -> evalCheck1 ex x
  | P (arg, body) -> evalCheck2 ex body
  | C (a, b) -> evalCheck2 ex a && evalCheck2 ex b


let rec __s1 (__s2 : lambda) (__s3 : string list) : bool =
  match __s2 with
  | V __s11 -> List.mem __s11 __s3
  | C (__s12, __s13) -> __s1 __s12 __s3 && __s1 __s13 __s3
  | P (__s14, __s15) -> __s1 __s15 (__s14 :: __s3)


let rec check (lambda : lambda) : bool =
  match lambda with
  | V x -> false
  | P (arg, body) -> __s1 lambda [ arg ]
  | C (a, b) -> check a && check b
