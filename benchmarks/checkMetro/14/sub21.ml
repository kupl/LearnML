type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string
;;

module StringSet = Set.Make(String);;

let checkMetro metro =
  (* 지역에 포함되지 않은 Station을 뽑아낸다. *)
  let rec excluded_stations metro =
    match metro with
    | STATION name ->
       StringSet.add name StringSet.empty
    | AREA (name, metro) ->
       (* Remove name from metro *)
       let station_set = excluded_stations metro in
       StringSet.remove name station_set
    | CONNECT (metro_a, metro_b) ->
       (* Union Set *)
       let set_a = excluded_stations metro_a in
       let set_b = excluded_stations metro_b in
       StringSet.union set_a set_b
  in

  StringSet.is_empty (excluded_stations metro)
;;
