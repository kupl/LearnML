type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec comparelist (procdata : 'b list) (vardata : 'b list) : 'a list =
  match (procdata, vardata) with
  | _, [] -> []
  | hd1 :: tl1, hd2 :: tl2 ->
      if hd1 = hd2 then comparelist procdata tl2
      else comparelist [ hd1 ] tl2 @ comparelist tl1 vardata
  | [], hd2 :: tl2 -> hd2 :: tl2


let rec varlist (lambda : lambda) : var list =
  match lambda with
  | V v -> [ v ]
  | P (_, a) -> varlist a
  | C (a, b) -> varlist a @ varlist b


let rec proclist (lambda : lambda) : var list =
  match lambda with
  | P (v, a) -> v :: proclist a
  | C (a, b) -> proclist a @ proclist b
  | _ -> []


let check (lambda : lambda) : bool =
  let list1 : string list = proclist lambda in

  let list2 : string list = varlist lambda in

  let list3 : string list = comparelist list1 list2 in
  if list3 = [] then true else false
