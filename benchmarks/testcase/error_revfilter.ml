let rec f check lst = 
    match lst with 
    | [] -> [] 
    | h :: t -> 
        if check h 
        then h :: f check t 
        else f check t ;;