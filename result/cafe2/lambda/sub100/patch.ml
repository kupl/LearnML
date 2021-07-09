type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let check (lambda : lambda) : bool =
  let rec make_list (lambda' : lambda)
      ((lst1 : string list), (lst2 : string list)) : var list * var list =
    match lambda' with
    | V v -> (lst1, v :: lst2)
    | P (v, e) -> make_list e (v :: lst1, lst2)
    | C (e1, e2) -> make_list e1 (make_list e2 (lst1, lst2))
  in

  let rec exist (lst : string list) (v : string) : bool =
    match lst with
    | [] -> false
    | hd :: tl -> if hd = v then true else exist tl v
  in

  let rec real_check ((lst1 : string list), (lst2 : string list)) : bool =
    match lst2 with
    | [] -> true
    | hd :: tl -> if exist lst1 hd then real_check (lst1, tl) else false
  in
  List.length (__s3 lambda) = 0
