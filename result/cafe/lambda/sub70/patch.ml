type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec ch ((e : lambda), (a : string)) : bool =
  match e with
  | V b -> if a = b then true else false
  | P (b, e) -> (
      match e with
      | V c -> if c = b || c = a then true else false
      | P (c, e) -> ch (e, c) || ch (e, b) || ch (e, a)
      | C (e1, e2) ->
          if a = b then ch (e1, a) && ch (e2, a)
          else (ch (e1, a) || ch (e1, b)) && (ch (e2, a) || ch (e2, b)) )
  | C (e1, e2) -> ch (e1, a) && ch (e2, a)


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool = List.length (__s3 e) = 0
