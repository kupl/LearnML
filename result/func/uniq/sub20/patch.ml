let rec uniq (lst : 'a list) : 'b list =
  let rec loop (l : 'a list) n : 'a list =
    match l with
    | [] -> l
    | hd :: tl -> if n = hd then loop tl n else hd :: loop tl n
  in

  match lst with [] -> lst | hd :: tl -> hd :: uniq (loop lst hd)


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
