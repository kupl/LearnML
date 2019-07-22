type metro =
  STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name =
  string


let checkMetro = function metroLine ->
  let rec lstContains lst areaName =
    match lst with
    | [] -> false
    | h::rest ->
      if h = areaName then true
      else (lstContains rest areaName)
  in
  let rec checkMetroArea = function
    | (metroLine, areaLst) ->
      (match metroLine with
      | STATION stationName ->
        lstContains areaLst stationName
      | AREA (areaName, subMetro) ->
        checkMetroArea(subMetro, areaLst@[areaName])
      | CONNECT (metro1, metro2) ->
        checkMetroArea(metro1, areaLst) &&
        checkMetroArea(metro2, areaLst))
  in
    checkMetroArea(metroLine, []);;
