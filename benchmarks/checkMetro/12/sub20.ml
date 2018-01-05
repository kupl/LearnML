(* Ex8 *)
type partial = FIRSTHALF
             | SECONDHALF
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
  and name = string


let rec checkMetro metro =
   match metro with
   | STATION(_) -> false
   | AREA(a, STATION(s)) -> if a = s then true
                            else false
   | AREA(a, AREA(a2, CONNECT(m1, m2))) -> checkMetro(AREA(a2,CONNECT(m1,m2))) 
                                        || (checkMetro(AREA(a, m1)) && checkMetro(AREA(a2, m2)))
										|| (checkMetro(AREA(a, m2)) && checkMetro(AREA(a2, m1)))
										|| (checkMetro(AREA(a, m1)) && checkMetro(AREA(a, m2)))
   | AREA(a, AREA(a2, m)) -> checkMetro(m) || checkMetro(AREA(a2, m)) || checkMetro(AREA(a, m))
   | AREA(a, CONNECT(m1, m2)) -> checkMetro(AREA(a, m1)) && checkMetro(AREA(a, m2))
   | CONNECT(STATION(_), _) -> false
   | CONNECT(_, STATION(_)) -> false
   | CONNECT(AREA(a1, m1), AREA(a2, m2)) -> checkMetro(AREA(a1,m1)) && checkMetro(AREA(a2, m2))
   | CONNECT(CONNECT(m1, m2), CONNECT(m3, m4)) -> checkMetro(m1) && checkMetro(m2) && checkMetro(m3) && checkMetro(m4)
   | CONNECT(CONNECT(m1, m2), AREA(a, m)) -> checkMetro(m1) && checkMetro(m2) && checkMetro(AREA(a, m))
   | CONNECT(AREA(a, m), CONNECT(m1, m2)) -> checkMetro(AREA(a, m)) && checkMetro(m1) && checkMetro(m2)