let rec removeElement : 'a -> 'a list -> 'a list =
 fun ele l ->
  match l with
  | [] -> []
  | h :: t -> if h = ele then removeElement ele t else h :: removeElement ele t


let rec app : 'a list -> 'a list -> 'a list =
 fun l1 l2 ->
  match l1 with [] -> l2 | h :: t -> app t (h :: removeElement h l2)


let uniq : 'a list -> 'a list =
 fun lst ->
  let lst2 = [] in

  match lst with [] -> lst2 | h :: t -> app lst lst2


let _ = uniq [ 5; 6; 5; 4; 4; 4; 4; 3; 3; 3 ]
