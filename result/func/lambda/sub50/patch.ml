type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isInclude ((n : string), (m : lambda)) : bool =
  match m with
  | V a -> n = a
  | P (a, b) -> isInclude (a, b) && isInclude (n, b)
  | C (a, b) -> isInclude (n, a) || isInclude (n, b)


let rec __s4 ((__s5 : lambda), (__s6 : string list)) : bool =
  match __s5 with
  | V __s7 -> List.exists (fun (__s8 : string) -> __s8 = __s7) __s6
  | P (__s9, __s10) -> __s4 (__s10, __s9 :: __s6)
  | C (__s11, __s12) -> __s4 (__s11, __s6) && __s4 (__s12, __s6)


let rec check (x : lambda) : bool =
  match x with P (a, V b) -> a = b | __s3 -> __s4 (x, []) | _ -> false
