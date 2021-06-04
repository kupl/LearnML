let rec removeElement ele (l : 'b list) : 'a list =
  match l with
  | [] -> []
  | h :: t -> if h = ele then removeElement ele t else h :: removeElement ele t


let rec app (l1 : 'c list) (l2 : 'c list) : 'a list =
  match l1 with [] -> l2 | h :: t -> app t (h :: removeElement h l2)


let uniq (lst : 'd list) : 'a list =
  let lst2 : 'd list = [] in

  match lst with [] -> lst2 | h :: t -> app lst lst2


let (_ : int list) = uniq [ 5; 6; 5; 4; 4; 4; 4; 3; 3; 3 ]
