type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro (m : metro) =   
    match m with
       AREA(a, STATION b) -> if a=b then true
                                        else false
     |AREA (a,(AREA(b, c))) -> if a=b then checkMetro (AREA(b,c))
                                         else if checkMetro (AREA(a,c))=true then true
                                         else if checkMetro (AREA(b,c))=true then true
                                         else false
     |AREA(a, (CONNECT(b,c))) -> if checkMetro(AREA(a,b))=true then
                                                         if checkMetro(AREA(a,c))=true then true
                                                         else false
                                                    else false ;;






type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro (m : metro) =   
    match m with
       AREA(a, STATION b) -> if a=b then true
                                        else false
     |AREA (a,(AREA(b, c))) -> if a=b then checkMetro(AREA(b,c))
                                         else if checkMetro(AREA(a,c))=true then checkMetro(AREA(a,c))
                                         else if checkMetro(AREA(b,c))=true then checkMetro(AREA(b,c))
                                         else false
     |AREA(a, (CONNECT(b,c))) -> if checkMetro(AREA(a,b))=true then
                                                         if checkMetro(AREA(a,c))=true then true
                                                         else false
                                                 else false
                                                
     |STATION(a) -> false
     |CONNECT(a,b) -> if checkMetro(a)=true then
                                    if checkMetro(b)=true then true
                                    else false
                                else false;;


checkMetro( AREA("a", 
                        AREA("b", 
                               CONNECT (AREA("c", AREA("d", STATION "a")),  AREA("e", STATION "b") )
                                 )
                          )
                 );;



      