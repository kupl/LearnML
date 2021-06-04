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


let rec __s1 (__s2 : lambda) : string list =
  match __s2 with
  | V __s8 -> [ __s8 ]
  | P (__s9, __s10) ->
      List.filter (fun (__s11 : string) -> __s11 != __s9) (__s1 __s10)
  | C (__s12, __s13) ->
      let __s14 : string list = __s1 __s12 in

      let __s15 : string list = __s1 __s13 in
      List.append __s14
        (List.filter (fun (__s16 : string) -> not (List.mem __s16 __s14)) __s15)


let check (lambda : lambda) : bool =
  let list1 : string list = proclist lambda in

  let list2 : string list = varlist lambda in

  let list3 : string list = comparelist list1 list2 in
  if __s1 lambda = [] then true else false
