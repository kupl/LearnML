let rec check_lst 
=fun lst a ->
  match lst with
    []->true
    |hd::tl-> if hd<>a then check_lst tl a
              else false;;
              
let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    []->[]
    |lst_hd::lst_tl-> 
      let rec append_lst lst_tl new_lst=
        match lst_tl with
          []->new_lst
          |hd::tl-> if check_lst new_lst hd then (append_lst tl (new_lst@[hd]))
                    else append_lst tl new_lst
                    
      in append_lst lst_tl [lst_hd];;
        
