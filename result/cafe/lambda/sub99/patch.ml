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


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (lambda : lambda) : bool = List.length (__s3 lambda) = 0
