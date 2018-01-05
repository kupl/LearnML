type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
  and name = string


let rec checkMetro metro =
    match metro with
        STATION(name) -> false
      | AREA(name1, STATION(name2)) -> (name1 = name2)
      | AREA(name1, AREA(name2, CONNECT(metro1, metro2))) ->
      				((checkMetro (AREA(name1, metro1))) & (checkMetro (AREA(name2, metro2)))) or
				((checkMetro (AREA(name1, metro2))) & (checkMetro (AREA(name2, metro1))))
      | AREA(name1, AREA(name2, metro)) -> 
      				((checkMetro (AREA(name1, metro))) or (checkMetro (AREA(name2, metro))))
      | AREA(name, CONNECT(metro1, metro2)) -> 
      				(checkMetro (AREA(name, metro1))) & (checkMetro (AREA(name, metro2)))
      | CONNECT(metro1, metro2) -> ((checkMetro metro1) & (checkMetro metro2))

