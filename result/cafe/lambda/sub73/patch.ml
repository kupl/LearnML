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


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool = List.length (__s3 e) = 0
