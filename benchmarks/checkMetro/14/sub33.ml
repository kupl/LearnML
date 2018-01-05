type metro = STATION of name
        |AREA of name*metro
        |CONNECT of metro*metro
and name = string

let checkMetro metro = 
        let rec _checkMetro(m,nList)= 
                match m with
                |STATION n1 -> (List.mem n1 nList)
                |AREA (n1,m1) -> _checkMetro(m1,[n1]@nList)
                |CONNECT (m1,m2) -> (_checkMetro(m1,nList) && _checkMetro(m2,nList))
        in _checkMetro(metro, [])
        
