let rec filter (f : 'a -> bool) (__fun__ : 'a list) : 'a list =
  match __fun__ with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | h :: t -> h :: uniq (filter (fun __s8 -> __s8 != h) t)


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
