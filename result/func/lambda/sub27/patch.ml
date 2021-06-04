type var = string

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

let rec apl ((b : string), (m : lambda)) : lambda =
  match (b, m) with
  | b, V a -> if b = a then P (b, V a) else V a
  | b, P (a, V c) -> if a != c && b = c then P (b, V c) else P (a, V c)
  | b, P (a, P (c, d)) -> P (a, P (c, apl (b, d)))
  | b, P (a, C (c, d)) -> P (a, apl (b, C (c, d)))
  | b, C (a, c) -> C (apl (b, a), apl (b, c))


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (m : lambda) : bool = List.length (__s3 m) = 0
