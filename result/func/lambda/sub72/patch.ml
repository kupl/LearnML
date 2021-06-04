type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s1 (__s2 : string) (__s3 : string list) : var list =
  match __s3 with
  | [] -> []
  | __s13 :: __s14 ->
      if __s13 = __s2 then __s1 __s2 __s14 else __s13 :: __s1 __s2 __s14


let check (ex : lambda) : bool =
  let rec var_list (ex2 : lambda) : var list =
    match ex2 with
    | V v -> [ v ]
    | P (v2, ex3) -> __s1 v2 (var_list ex3)
    | C (ex3, ex4) -> var_list ex3 @ var_list ex4
  in

  let rec proc_list (ex2 : lambda) : var list =
    match ex2 with
    | V v -> []
    | P (v2, ex3) -> proc_list ex3
    | C (ex3, ex4) -> proc_list ex3 @ proc_list ex4
  in

  let rec _in_ ((vlist : string list), (v : string)) : bool =
    match vlist with
    | [] -> false
    | hd :: tl -> if hd = v then true else _in_ (tl, v)
  in

  let rec check2 ((vlist : string list), (plist : string list)) : bool =
    match vlist with
    | [] -> true
    | hd :: tl -> _in_ (plist, hd) && check2 (tl, plist)
  in
  check2 (var_list ex, proc_list ex)
