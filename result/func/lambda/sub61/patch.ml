type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec chars (e : lambda) : var =
  match e with
  | V a -> a
  | P (v, e1) -> (
      match e1 with V b -> b | P (b, e2) -> chars e2 | C (e1, e2) -> chars e2 )
  | C (e1, e2) -> (
      match (e1, e2) with V b, V c -> c | V b, P (c, e3) -> chars e3 )


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool = List.length (__s3 e) = 0
