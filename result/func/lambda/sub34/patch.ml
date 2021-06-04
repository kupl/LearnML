type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec deletecity (var, (lis : 'a list)) : 'a list =
  match lis with
  | [] -> []
  | __s14 :: __s15 ->
      if __s14 = var then deletecity (var, __s15)
      else __s14 :: deletecity (var, __s15)


let rec findcity (met : lambda) : string list =
  match met with
  | V var -> [ var ]
  | P (var, lambda) -> deletecity (var, findcity lambda)
  | C (met1, met2) -> List.rev_append (findcity met1) (findcity met2)


let deletestation (var, (lambda : lambda)) : bool =
  let citylist : string list = findcity lambda in
  if deletecity (var, citylist) = [] then true else false


let rec check (met : lambda) : bool =
  match met with
  | V var -> false
  | P (var, lambda) -> if check lambda then true else deletestation (var, lambda)
  | C (met1, met2) -> check met1 && check met2
