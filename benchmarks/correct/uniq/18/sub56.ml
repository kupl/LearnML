let rec uniq : 'a list -> 'a list
= fun lst ->
    let rec contains e l2 =
        match l2 with
        | x :: xs -> if x = e then true else contains e xs
        | [] -> false in
    let rec uniq' s l2 =
        match l2 with
        | x :: xs ->
            if contains x s then uniq' s xs
            else x :: uniq' (x :: s) xs
        | [] -> [] in
    uniq' [] lst
;;

uniq [5;6;6;4];;
