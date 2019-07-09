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
 

let rec remove_dup lst h
= match lst with
  | [] -> [h]
  | hd :: tl -> if h = hd then lst else hd::remove_dup tl h
;;

let rec app: 'a list -> 'a list -> 'a list
= fun l1 l2->
  match l1 with 
  |[] ->uniq l2(*l2에 원래 중복되는 요소들을 빼주기 위하여*)
  |hd::tl->  app tl (remove_dup l2 hd)

;;    


      
app [4;5;6;7;-1;-3] [1;2;3;4;8;8;8];;
app [1;2;3][4;4;4];;    
app [4;5;6;7] [1;2;3;4];;
app [1;1;1;2][3;3;2];;
(*match l2 with
        []->[]
        |h::t-> h::remove_dup_sec l2 h
        *)