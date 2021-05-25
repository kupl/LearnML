let uniq : 'a list -> 'a list
= fun lst -> 
  let rec search key lst = 
    match lst with
      | [] -> false
      | hd::tl -> if key = hd then true else search key tl
  
  in let rec mklist result lst = 
       match lst with
         | [] -> result
         | hd::tl -> 
           if (search hd result) = true then mklist result tl
           else let new_result = result@[hd]
                in mklist new_result tl
    
     in (mklist [] lst);;


(*output*)
uniq [5;6;5;4;4;4;4;4;4;5;5;5;5;5;6;6;6;66;2;2;2;2;1;1;1;1;1;0;0;0;0;0;5;5;5;5;8;8;8;9];;
