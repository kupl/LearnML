let rec app : 'a list -> 'a list -> 'a list
= fun l2 l1 -> 
    let original2 = l2 in
    let rec length lst =
      match lst with
        [] -> 0
        |(_::xs) -> 1 + length xs in
    let rec findidx lst1 lst2 count =
      match lst1, lst2 with
        [], _ -> count
        |(x::xs), (y::ys) -> if x = y then findidx xs ys (count + 1) else findidx xs (y::ys) count
        |xs, [] -> findidx xs original2 0 in
    let rec take count lst =
      match count with
        0 -> []
        |n -> match lst with
          [] -> raise (Failure "Wrong argument.")
          |(x::xs) -> x :: (take (n-1) xs) in
    (take ((length l1) - (findidx l1 l2 0)) l1) @ l2;;
    
    
app [4;5;6] [1;2;4;5;6;4;5];;
app [4;5;6;7] [1;2;3;4];;
