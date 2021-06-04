type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let isStringExist (e : string) (l : 'a list) : bool =
  let x = List.find (fun x -> x = e) l in
  true


let rec __s3 (__s4 : var list) (__s5 : lambda) : bool =
  match __s5 with
  | V __s6 -> List.mem __s6 __s4
  | P (__s7, __s8) ->
      if List.mem __s7 __s4 then __s3 __s4 __s8 else __s3 (__s7 :: __s4) __s8
  | C (__s9, __s10) -> __s3 __s4 __s9 && __s3 __s4 __s10


let rec check (lambda : lambda) : bool =
  let rec checkWithArea (areaList : string list) (lambda : lambda) : bool =
    match lambda with
    | V var -> isStringExist var areaList
    | P (var, lambda) -> checkWithArea (areaList @ [ var ]) lambda
    | C (m1, m2) -> checkWithArea areaList m1 && checkWithArea areaList m2
  in
  __s3 [] lambda
