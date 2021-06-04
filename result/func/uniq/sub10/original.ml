let rec ifequal n (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> hd = n || ifequal n tl


let rec uniq (lst : 'b list) : 'c list =
  let rec l (lst : 'b list) (result : 'b list) : 'b list =
    match lst with
    | [] -> result
    | hd :: tl -> if ifequal hd tl then l tl result else l tl (hd :: result)
  in
  l lst []


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
