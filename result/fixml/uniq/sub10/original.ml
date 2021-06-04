let rec ifequal n l =
  match l with [] -> false | hd :: tl -> hd = n || ifequal n tl


let rec uniq : 'a list -> 'a list =
 fun lst ->
  let rec l lst result =
    match lst with
    | [] -> result
    | hd :: tl -> if ifequal hd tl then l tl result else l tl (hd :: result)
  in
  l lst []


let _ = uniq [ 5; 6; 5; 4 ]
