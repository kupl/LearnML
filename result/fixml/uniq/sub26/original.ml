let rec uniq l1 =
  let rec checknumber l2 i =
    match l2 with
    | [] -> []
    | hd :: tl ->
        if i = hd then i :: checknumber tl i else hd :: checknumber tl i
  in

  match l1 with [] -> [] | hd :: tl -> checknumber tl hd


let _ = uniq [ 5; 6; 5; 4 ]
