type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec str (lambda : lambda) : string list =
  match lambda with
  | V a -> []
  | P (a, e1) -> a :: str e1
  | C (e1, e2) -> str e1 @ str e2


let rec v_str (lambda : lambda) : string list =
  match lambda with
  | V a -> [ a ]
  | P (a, e1) -> v_str e1
  | C (e1, e2) -> v_str e1 @ v_str e2


let rec search var (lambda : 'a list) : bool =
  match lambda with
  | [] -> false
  | hd :: tl -> if hd = var then true else search var tl


let rec compare (e1 : 'b list) (e2 : 'b list) : bool =
  match e2 with
  | [] -> true
  | hd :: tl -> if search hd e1 && compare e1 tl then true else false


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let check (lambda : lambda) : bool = List.length (__s3 lambda) = 0
