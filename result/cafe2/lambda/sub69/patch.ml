type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let insert a (l : 'a list) : 'a list =
  match l with [] -> [ a ] | hd :: tl -> a :: hd :: tl


let rec confirm a (l : 'b list) : bool =
  match l with [] -> false | hd :: tl -> if hd = a then true else confirm a tl


let rec cal ((a : string list), (b : lambda)) : string list =
  match b with
  | V v -> if confirm v a then a else insert "false" a
  | P (x, y) -> if confirm x a then cal (a, y) else cal (insert x a, y)
  | C (x, y) -> cal (cal (a, x), y)


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let check (e : lambda) : bool = List.length (__s3 e) = 0
