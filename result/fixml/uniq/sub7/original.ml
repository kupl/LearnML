let rec check x lst =
  match lst with [] -> false | hd :: tl -> hd = x || check x tl


let rec uniq : 'a list -> 'a list =
 fun lst ->
  let rec f lst out =
    match lst with
    | [] -> out
    | hd :: tl -> if check hd out then f tl out else f tl ([ hd ] @ out)
  in
  f lst []


let _ = uniq [ 5; 6; 5; 4 ]

let _ = uniq [ 4; 4; 5; 6; 7; 7; 8; 8 ]
