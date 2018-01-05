
(* exercise 4 not yet*)
type name = string
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro

let checkMetro largein =
        let rec metrorec smallin l = 
                match smallin with
                | STATION(x) -> (List.mem x l)
                | AREA(x, y) -> (metrorec y (x::l))
                | CONNECT(y, z) -> ((metrorec y l) && (metrorec z l))
         in
         metrorec largein []
