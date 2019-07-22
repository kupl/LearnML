type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string
;;

let checkMetro : metro -> bool = fun met ->
  let rec checkMetroIn : metro * name list -> bool = fun (metro, stationlist) ->
    match metro with
    | STATION(stationstr) -> if (List.mem stationstr stationlist) then true else false
    | AREA(areastr,metin) -> checkMetroIn (metin,stationlist@[areastr] )
    | CONNECT(metin1,metin2) -> (checkMetroIn (metin1,stationlist))&&(checkMetroIn (metin2,stationlist))
  in
  match met with
  | STATION(namestr) -> true
  | _ -> checkMetroIn(met,[])
;;
