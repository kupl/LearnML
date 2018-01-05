type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
  and name = string

let checkMetro metro =
  let rec iter target names =
    match target with
    | STATION station_name ->
      (List.length (List.filter (fun x -> x = station_name) names)) > 0
    | AREA (area_name, new_target) -> iter new_target (area_name::names)
    | CONNECT (metro_a, metro_b) -> (iter metro_a names)
                                    && (iter metro_b names)
  in
    iter metro []
