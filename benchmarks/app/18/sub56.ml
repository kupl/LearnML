let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
    let rec contains e lst =
        match lst with
        | x :: xs -> if x = e then true else contains e xs
        | [] -> false
    in match l1 with
    | x :: xs ->
        let l3 = if contains x l2 then l2 else l2 @ [x]
        in app xs l3
    | [] -> l2
;;

app [4;5;6;7] [1;2;3;4];;
