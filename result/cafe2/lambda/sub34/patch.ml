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
  | P (var, lambda) ->
      List.filter (fun (__s9 : string) -> __s9 != var) (findcity lambda)
  | C (met1, met2) -> List.rev_append (findcity met1) (findcity met2)


let deletestation (var, (lambda : lambda)) : bool =
  let citylist : string list = findcity lambda in
  if deletecity (var, citylist) = [] then true else false


let rec check (met : lambda) : bool =
  match met with
  | V var -> false
  | P (var, lambda) -> if check lambda then true else deletestation (var, lambda)
  | C (met1, met2) -> check met1 && check met2
