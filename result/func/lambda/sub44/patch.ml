type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let isStringExist (e : string) (l : 'a list) : bool =
  let x = List.find (fun x -> x = e) l in
  true


let rec __s1 (__s2 : 'b list) __s3 : bool =
  match __s2 with
  | [] -> false
  | __s19 :: __s20 -> if __s19 = __s3 then true else __s1 __s20 __s3


let rec check (lambda : lambda) : bool =
  let rec checkWithArea (areaList : string list) (lambda : lambda) : bool =
    match lambda with
    | V var -> __s1 areaList var
    | P (var, lambda) -> checkWithArea (areaList @ [ var ]) lambda
    | C (m1, m2) -> checkWithArea areaList m1 && checkWithArea areaList m2
  in
  checkWithArea [] lambda
