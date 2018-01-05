type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro m =    
    let rec idCheck id_list m =
        let rec exists f l = match l with
            | [] -> false
            | h::t -> if f=h then true else exists f t in
        
        match m with
              STATION a -> (exists a id_list)
              |AREA (id1, m1) -> idCheck (id1::id_list) m1
              |CONNECT (m1,m2) -> (idCheck id_list m1)&&(idCheck id_list m2)
    in

    match m with
          AREA (name, metro) -> idCheck (name::[]) metro
          |_ -> false
          
           
    
