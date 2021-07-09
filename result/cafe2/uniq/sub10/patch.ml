let rec ifequal n (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> hd = n || ifequal n tl


let rec __s3 __s4 (__s5 : 'b list) : bool =
  match __s5 with
  | [] -> true
  | __s11 :: __s12 -> if __s4 = __s11 then false else __s3 __s4 __s12


let rec uniq (lst : 'c list) : 'd list =
  let rec l (lst : 'c list) (result : 'c list) : 'c list =
    match lst with
    | [] -> result
    | hd :: tl -> if __s3 hd result then l tl (result @ [ hd ]) else l tl result
  in
  l lst []


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
