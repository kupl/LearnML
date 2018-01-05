type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

module Location = Set.Make(String);;

let rec checkMetro2 met set =
  match met with
  |STATION name -> Location.mem name set
  |AREA (name, metro) -> checkMetro2 metro (Location.add name set)
  |CONNECT (metro1, metro2) -> if(checkMetro2 metro1 set) = false then false
                              else if(checkMetro2 metro2 set) = false then false
                              else true
  ;;

let checkMetro met =
  checkMetro2 met Location.empty
;;


