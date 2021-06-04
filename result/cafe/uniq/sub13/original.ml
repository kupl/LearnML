let rec elm (lst : 'a list) a : 'a list =
  match lst with
  | [] -> []
  | hd :: tl -> if hd = a then elm tl a else hd :: elm tl a


let rec uniq (lst : 'c list) : 'b list =
  [] @ match lst with [] -> [] | hd :: tl -> hd :: elm tl hd


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
