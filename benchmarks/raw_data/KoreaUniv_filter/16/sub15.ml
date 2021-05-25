let rec filter pred lst  
  =  match lst with
    |[] -> raise(Failure "NO item")
    |[a] -> if pred a then [a] else [] 
    |hd::tl -> if  pred hd then hd :: filter pred tl else filter pred tl;;
