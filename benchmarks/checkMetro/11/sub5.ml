type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro a =

        match a with
        |STATION _  -> raise (Invalid_argument "checkMetro")
        |CONNECT(a,b) -> raise (Invalid_argument "checkMetro")
        

        |AREA(a, STATION b) -> if a=b then true
                                  else false
        
        |AREA(a, CONNECT(STATION b, STATION c)) -> if a=b || a=c then true
                                                   else false
        
        |AREA(a, CONNECT(AREA (b, STATION c), STATION d)) -> if a=c || a=d then true
                                                                 else false

        |AREA(a, CONNECT(STATION b, AREA(c, STATION d))) -> if a=b|| a=d
                                                                then true
                                                                else false
       

        |AREA(a, b) -> checkMetro b
