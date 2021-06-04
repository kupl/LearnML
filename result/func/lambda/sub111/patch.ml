type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec comparelist (procdata : 'b list) (vardata : 'b list) : 'a list =
  match (procdata, vardata) with
  | _, [] -> []
  | hd1 :: tl1, hd2 :: tl2 ->
      if hd1 = hd2 then comparelist procdata tl2
      else comparelist [ hd1 ] tl2 @ comparelist tl1 vardata
  | [], hd2 :: tl2 -> hd2 :: tl2


let rec __s1 (__s2 : string) (__s3 : string list) : var list =
  match __s3 with
  | [] -> []
  | __s13 :: __s14 ->
      if __s13 = __s2 then __s1 __s2 __s14 else __s13 :: __s1 __s2 __s14


let rec varlist (lambda : lambda) : var list =
  match lambda with
  | V v -> [ v ]
  | P (__s9, __s10) -> __s1 __s9 (varlist __s10)
  | P (_, a) -> varlist a
  | C (a, b) -> varlist a @ varlist b


let rec proclist (lambda : lambda) : var list =
  match lambda with
  | P (v, a) -> __s1 v (proclist a)
  | C (a, b) -> proclist a @ proclist b
  | _ -> []


let check (lambda : lambda) : bool =
  let list1 : string list = proclist lambda in

  let list2 : string list = varlist lambda in

  let list3 : string list = comparelist list1 list2 in
  if list3 = [] then true else false
