type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : string) (__s5 : string list) : string list =
  match __s5 with
  | __s14 :: __s15 ->
      if __s14 = __s4 then __s3 __s4 __s15 else __s14 :: __s3 __s4 __s15
  | [] -> []


let rec input_var (e : lambda) (lst : string list) : string list =
  match e with
  | V v -> lst
  | P (v, e1) -> input_var e1 lst @ [ v ]
  | C (e1, e2) -> input_var e1 (input_var e2 lst)


let rec input_lambda (e : lambda) (lst : string list) : string list =
  match e with
  | V v -> lst @ [ v ]
  | P (v, e1) -> input_lambda e1 lst
  | C (e1, e2) -> input_lambda e1 (input_lambda e2 lst)


let rec isthere (lst : string list) (l2 : string list) : bool =
  match lst with
  | [] -> true
  | hd :: tl -> (
      match l2 with
      | [] -> false
      | h :: t -> if hd = h then true else isthere [ hd ] t )


let rec __s6 (__s7 : lambda) (__s8 : string list) : string list =
  match __s7 with
  | V __s9 -> __s9 :: __s8
  | P (__s10, __s11) -> __s3 __s10 (__s6 __s11 __s8)
  | C (__s12, __s13) -> __s6 __s12 __s8 @ __s6 __s13 __s8


let rec check (lambda : lambda) : bool =
  let l2 : string list = input_var lambda [] in

  let l1 : string list = input_lambda lambda [] in
  if __s6 lambda [] = [] then true else false
