type lambda =
  V of var
  | P of var * lambda
  | C of lambda * lambda
and var =
  string


let check = function lambdaLine ->
  let rec lstContains lst areaName =
    match lst with
    | [] -> false
    | h::rest ->
      if h = areaName then true
      else (lstContains rest areaName)
  in
  let rec checkArea = function
    | (lambdaLine, areaLst) ->
      (match lambdaLine with
      | V stationName ->
        lstContains areaLst stationName
      | P (areaName, subMetro) ->
        checkArea(subMetro, areaLst@[areaName])
      | C (lambda1, lambda2) ->
        checkArea(lambda1, areaLst) &&
        checkArea(lambda2, areaLst))
  in
    checkArea(lambdaLine, []);;
