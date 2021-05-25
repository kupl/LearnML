let rec power a b =
  if(a = 1) then b * 1
  else (b * (power (a-1) b));;
  

let rec lst2int : int list -> int
= fun lst ->
  match lst with 
    | [] -> 0
    | hd::tl ->
      if(tl = []) then hd
      else
        (hd * (power (List.length tl) 10)) + lst2int(tl);;
        
lst2int [2;3;4;5];;
