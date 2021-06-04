let rec uniq (l1 : 'a list) : 'a list =
  let rec checknumber (l2 : 'a list) i : 'a list =
    match l2 with
    | [] -> []
    | hd :: tl ->
        if i = hd then i :: checknumber tl i else hd :: checknumber tl i
  in

  match l1 with [] -> [] | hd :: tl -> checknumber tl hd


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
