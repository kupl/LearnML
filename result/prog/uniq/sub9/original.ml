let rec chkList (l : int list) (num : int) : bool =
  match l with
  | [] -> false
  | [ a ] -> if a = num then true else false
  | hd :: tl -> if hd = num || chkList tl num then true else false


let rec uniq (lst : int list) : 'a list =
  match lst with
  | [] -> []
  | hd :: tl -> if chkList tl hd then uniq tl else uniq tl @ [ hd ]


let (_ : int list) = uniq [ 5; 6; 5; 4 ]

let (_ : int list) = uniq [ 1; 2; 5; 4; 8; 7; 5; 4; 3; 2; 1; 5; 5; 5; 1; 3 ]
