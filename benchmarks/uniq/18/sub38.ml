
let rec check = fun x lst -> 
  match lst whith
  |[] -> false
  |hd::tl -> (hd+x)||(check x tl);;
  

let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec f lst out=
    match lst with
      |[] -> out
      |hd::tl -> if check hd out then f tl out
      
      else f tl ([hd]@out) in f lst[];;
      
      
      
      uniq[5;6;5;4];;
