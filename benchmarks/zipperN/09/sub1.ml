let rec zipperN (lst: int list list) =
    let getHeadtoint(lst: int list list) =
        match lst with
           h::t -> h 
         | []  -> []
    in
    let getHeadtolist(lst) =
        match lst with
           h::t -> [h]
         | []  -> []
    in
    let getTail(lst) =
         match lst with
           h::t -> t
          |[]  -> []
    in
    
    if lst = [] then
          []
    else
        if getTail(getHeadtoint(lst))=[] then
           getHeadtolist(getHeadtoint(lst))@zipperN(getTail(lst))
        else
           getHeadtolist(getHeadtoint(lst))@zipperN(getTail(lst)@[getTail(getHeadtoint(lst))]);;
