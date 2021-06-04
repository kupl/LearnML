let rec elm lst a =
  match lst with
  | [] -> []
  | hd :: tl -> if hd = a then elm tl a else hd :: elm tl a


let rec uniq : 'a list -> 'a list =
 fun lst -> [] @ match lst with [] -> [] | hd :: tl -> hd :: elm tl hd


let _ = uniq [ 5; 6; 5; 4 ]
