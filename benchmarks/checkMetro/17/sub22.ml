type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string


let rec check_list((l : name list), id) : bool =
  match l with
  |[] -> false
  |hd::tl -> if (hd <> id) then check_list(tl, id)
             else  true;;
 let rec checkMetro_list((l : name list), (m : metro)) : bool = 
  match m with
  | STATION(id) -> if check_list(l, id) then true
                   else false
  | AREA(id, m) -> checkMetro_list(id::l , m)
  | CONNECT(m1, m2) -> checkMetro_list(l, m1) && checkMetro_list(l, m2);;

  let rec checkMetro(m: metro) : bool =
    match m with
    | STATION(id) -> false
    | AREA(id, m) -> checkMetro_list(id::[], m)
    | CONNECT(m1, m2) -> checkMetro(m1) && checkMetro(m2);;
    