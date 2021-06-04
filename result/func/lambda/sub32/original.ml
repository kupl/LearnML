type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let addToList (lst : 'a list) newInlst : 'a list = newInlst :: lst

let rec check_sub (prevAreaName : string list) (lambda : lambda) : bool =
  match lambda with
  | V stationName -> List.mem stationName prevAreaName
  | P (areaName, nextMetro) -> check_sub (areaName :: prevAreaName) nextMetro
  | C (lambdaA, lambdaB) ->
      check_sub prevAreaName lambdaA && check_sub prevAreaName lambdaB


let rec check (inpt : lambda) : bool =
  match inpt with
  | V var -> true
  | P (var, lambda) -> check_sub [ var ] lambda
  | C (lambdaA, lambdaB) -> check lambdaA && check lambdaB
