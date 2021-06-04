type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (m : lambda) : bool = List.length (__s3 m) = 0

and listMetro (m : lambda) : 'a list =
  match m with
  | V m' -> [ m' ]
  | C (m1, m2) -> (
      match (listMetro m1, listMetro m2) with
      | a :: b, c :: d -> List.append (a :: b) (c :: d)
      | _, _ -> [] )
  | P (n', m') -> ( match check m with true -> listMetro m' | false -> [] )
