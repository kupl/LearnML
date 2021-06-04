type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isInclude ((n : string), (m : lambda)) : bool =
  match m with
  | V a -> n = a
  | P (a, b) -> isInclude (a, b) && isInclude (n, b)
  | C (a, b) -> isInclude (n, a) || isInclude (n, b)


let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match __s4 with
  | V __s11 -> List.mem __s11 __s5
  | C (__s12, __s13) -> __s3 (__s12, __s5) && __s3 (__s13, __s5)
  | P (__s14, __s15) -> __s3 (__s15, List.append __s5 [ __s14 ])


let rec check (x : lambda) : bool =
  match x with
  | P (a, V b) -> a = b
  | P (a, b) -> __s3 (x, [])
  | C (__s7, __s8) -> check __s7 && check __s8
  | _ -> false
