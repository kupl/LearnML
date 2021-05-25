
type var =string
type lambda =V of var | P of var*lambda |C of lambda* lambda

let check e =
    let rec check_aux f =
    match f with 
	|V(a),l-> if l =[] then false 
                        else if a = List.hd l then true 
                         else check_aux(V(a),List.tl l)
        |P(a,b),l -> check_aux(b,a::l)  
	|C(a,b),l->check_aux(a,l)&&check_aux(b,l) 
       
     in check_aux(e,[])