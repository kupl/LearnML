let rec removeElement ele (l : 'b list) : 'a list =
  match l with
  | [] -> []
  | h :: t -> if h = ele then removeElement ele t else h :: removeElement ele t


let rec __s1 (__s2 : int) (__s3 : int list) : int list =
  match __s3 with
  | [] -> [ __s2 ]
  | __s11 :: __s12 -> if __s2 = __s11 then [] else __s1 __s2 __s12


let rec app (l1 : 'c list) (l2 : 'c list) : 'a list =
  match l1 with [] -> l2 | h :: t -> app t (l2 @ __s1 h l2)


let uniq (lst : 'd list) : 'a list =
  let lst2 : 'd list = [] in

  match lst with [] -> lst2 | h :: t -> app lst lst2


let (_ : int list) = uniq [ 5; 6; 5; 4; 4; 4; 4; 3; 3; 3 ]
