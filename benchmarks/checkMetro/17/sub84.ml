type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string
  
  let rec checkMetro2: metro * name list -> bool = function 
    (current_metro, area_list) -> 
      match current_metro with
        | AREA(area_name, sub_metro) -> checkMetro2(sub_metro, area_name::area_list)
        | CONNECT(sub_metro_1, sub_metro_2) -> checkMetro2(sub_metro_1, area_list) && checkMetro2(sub_metro_2, area_list)
        | STATION(station_name) -> List.mem station_name area_list
          
          
  let checkMetro: metro -> bool = function(_metro) -> checkMetro2(_metro, [])
    