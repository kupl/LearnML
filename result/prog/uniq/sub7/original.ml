let rec check x (lst : 'a list) : bool =
  match lst with [] -> false | hd :: tl -> hd = x || check x tl


let rec uniq (lst : 'b list) : 'c list =
  let rec f (lst : 'b list) (out : 'b list) : 'b list =
    match lst with
    | [] -> out
    | hd :: tl -> if check hd out then f tl out else f tl ([ hd ] @ out)
  in
  f lst []


let (_ : int list) = uniq [ 5; 6; 5; 4 ]

let (_ : int list) = uniq [ 4; 4; 5; 6; 7; 7; 8; 8 ]
