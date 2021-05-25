let app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], hd :: tl -> loop (hd :: acc) [] tl
    | hd :: tl, l -> loop (hd :: acc) tl l
    in
    loop [] l1 l2
    ;;
