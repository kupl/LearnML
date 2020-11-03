let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    []->[]
    |h::t-> h :: uniq( 
      let rec dump x ls = 
        match ls with 
          | [] -> []
          | hd::tl-> if x=hd then dump x tl else hd::dump x tl
      in dump h t);; 
 
  

uniq [5;6;5;4];;
uniq [4;4;4;4];;
uniq [5;5;7;7];;
uniq [5;7;5;7];;
uniq [];;
uniq [1];;
(*uniq [5;6;5;4] = [5;6;4]*)