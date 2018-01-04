let rec zipperN (list: int list list) =
    let getHeadtoint(list: int list list) =
        match list with
           h::t -> h 
         | []  -> []
    in
    let getHeadtolist(list) =
        match list with
           h::t -> [h]
         | []  -> []
    in
    let getTail(list) =
         match list with
           h::t -> t
          |[]  -> []
    in
    
    if list = [] then
          []
    else
        if getTail(getHeadtoint(list))=[] then
           getHeadtolist(getHeadtoint(list))@zipperN(getTail(list))
        else
           getHeadtolist(getHeadtoint(list))@zipperN(getTail(list)@[getTail(getHeadtoint(list))]);;
