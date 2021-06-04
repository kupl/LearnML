type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec append_list (list1 : 'a list) (list2 : 'a list) : 'a list =
  match list1 with [] -> list2 | hd :: tl -> hd :: append_list tl list2


let rec __s3 (__s4 : string) (__s5 : string list) : string list =
  match __s5 with
  | __s14 :: __s15 ->
      if __s14 = __s4 then __s3 __s4 __s15 else __s14 :: __s3 __s4 __s15
  | [] -> []


let rec get_variables (lambda : lambda) (result : string list) : string list =
  match lambda with
  | V v -> v :: result
  | P (v, e) -> __s3 v (get_variables e result)
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


let check (lambda : lambda) : bool =
  let variables : string list = get_variables lambda [] in
  if get_variables lambda [] = [] then true else false
