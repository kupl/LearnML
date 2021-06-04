type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s1 (__s2, (__s3 : 'a list), (__s4 : 'a list)) : 'a list =
  match __s4 with
  | [] -> __s3
  | __s16 :: __s17 ->
      if __s2 = __s16 then __s1 (__s2, __s3, __s17)
      else __s1 (__s2, __s16 :: __s3, __s17)


let rec __s5 (__s6 : lambda) : string list =
  match __s6 with
  | V __s10 -> [ __s10 ]
  | P (__s11, __s12) ->
      let __s13 : string list = __s5 __s12 in
      __s1 (__s11, [], __s13)
  | C (__s14, __s15) -> __s5 __s14 @ __s5 __s15


let check (lam : lambda) : bool =
  let myvarlst : string list = [] and myfreevarlst : string list = [] in

  let rec check2 (lambda : lambda) : string list =
    match lambda with
    | V v -> myfreevarlst @ [ v ]
    | P (v, l) ->
        let dummy1 : string list = myvarlst @ [ v ] in
        __s5 lam
    | C (l1, l2) ->
        let dummy2 : string list = check2 l1 in
        check2 l2
  in

  let rec check3 (varlst : string list) (freevarlst : string list) : bool =
    match freevarlst with
    | [] -> true
    | var :: tl -> (
        match varlst with
        | [] -> false
        | var2 :: tl2 ->
            if var = var2 then check3 tl freevarlst else check3 varlst tl2 )
  in

  let dummy : string list = check2 lam in
  if dummy = [] then true else false
