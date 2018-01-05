
type name =string
type metro =STATION of name | AREA of name*metro |CONNECT of metro* metro

let checkMetro e =
    let rec checkMetro_aux f =
    match f with 
	|STATION(a),l-> if l =[] then false 
                        else if a = List.hd l then true 
                         else checkMetro_aux(STATION(a),List.tl l)
        |AREA(a,b),l -> checkMetro_aux(b,a::l)  
	|CONNECT(a,b),l->checkMetro_aux(a,l)&&checkMetro_aux(b,l) 
       
     in checkMetro_aux(e,[])