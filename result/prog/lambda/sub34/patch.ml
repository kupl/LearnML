type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec deletecity (var, (lis : 'a list)) : 'a list =
  if var = List.hd lis then
    if List.length lis = 1 then [] else deletecity (var, List.tl lis)
  else if List.length lis = 1 then lis
  else List.hd lis :: deletecity (var, List.tl lis)


let rec findcity (met : lambda) : string list =
  match met with
  | V var -> [ var ]
  | P (var, lambda) -> deletecity (var, findcity lambda)
  | C (met1, met2) -> List.rev_append (findcity met1) (findcity met2)


let deletestation (var, (lambda : lambda)) : bool =
  let citylist : string list = findcity lambda in
  if deletecity (var, citylist) = [] then true else false


let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match (__s4, __s5) with
  | V __s11, __s12 -> List.mem __s11 __s12
  | P (__s13, __s14), __s15 -> __s3 (__s14, List.append [ __s13 ] __s15)
  | C (__s16, __s17), __s18 -> __s3 (__s16, __s18) && __s3 (__s17, __s18)


let rec check (met : lambda) : bool =
  match met with
  | V var -> false
  | P (var, lambda) -> __s3 (met, [ var ])
  | C (met1, met2) -> check met1 && check met2
