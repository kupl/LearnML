type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

  let rec checkMetro: metro -> bool = fun x -> match x with
    | AREA(n, STATION(n1)) -> if( n1 = n) then true
    else false
    | AREA(n, AREA(n1, m)) -> checkMetro(AREA(n,m)) && checkMetro(AREA(n1,m))
    | AREA(n, CONNECT(STATION(m1),STATION(m2))) -> if((n = m1) || n = m2) then true
    else false
    | AREA(n, CONNECT(AREA(n1,m1), STATION(n2))) -> if((n = n2 || n1 = n2) && (checkMetro(AREA(n,m1))||checkMetro(AREA(n1,m1)))) then true
    else false
    | AREA(n, CONNECT( STATION(n2), AREA(n1,m1))) -> if((n = n2 || n1 = n2) && (checkMetro(AREA(n,m1))||checkMetro(AREA(n1,m1)))) then true
    else false
    | CONNECT(AREA(n1,m1),AREA(n2,m2)) -> if((checkMetro(AREA(n1,m1)) || checkMetro(AREA(n2,m1))) && (checkMetro(AREA(n1,m2)) || checkMetro(AREA(n2,m2)))) then true
    else false
    | STATION(n) -> false
