let rec zipper((list1 : int list), (list2 : int list)) =
    let getHead(list) =
        match list with
           h::t -> [h]
         | []  -> []
    in
    let getTail(list) =
         match list with
           h::t -> t
          |[]  -> []
    in
    
    if list1 = [] then
         if list2 = [] then
             []
         else
             getHead(list1)@getHead(list2)@zipper(getTail(list1), getTail(list2))
    else
       getHead(list1)@getHead(list2)@zipper(getTail(list1), getTail(list2));;

