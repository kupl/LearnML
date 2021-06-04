type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec append_list (list1 : 'a list) (list2 : 'a list) : 'a list =
  match list1 with [] -> list2 | hd :: tl -> hd :: append_list tl list2


let rec get_variables (lambda : lambda) (result : string list) : string list =
  match lambda with
  | V v -> v :: result
  | P (v, e) -> get_variables e (v :: result)
  | C (e1, e2) ->
      append_list (get_variables e1 result) (get_variables e2 result)


let rec var_in_lambda (variable : string) (lambda : lambda) : bool =
  match lambda with
  | V v -> false
  | P (v, e) -> if v = variable then true else var_in_lambda variable e
  | C (e1, e2) -> var_in_lambda variable e1 || var_in_lambda variable e2


let rec check_helper (variables : string list) (lambda : lambda) : bool =
  match variables with
  | [] -> true
  | hd :: tl ->
      if var_in_lambda hd lambda = false then false else check_helper tl lambda


let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let __s6 (__s7 : lambda * string list) : bool =
  match __s7 with
  | V __s8, __s9 -> List.mem __s8 __s9
  | P (__s10, __s11), __s12 -> false
  | C (__s13, __s14), __s15 -> false


let check (lambda : lambda) : bool =
  let variables : string list = get_variables lambda [] in
  List.for_all __s6 (__s3 (lambda, []))
