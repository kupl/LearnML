let rec dup_checker n lst=
   match lst with
     |[]->false
     |hd::tl->
       if hd=n then true
       else dup_checker n tl
       
let rec reverse lst=
  match lst with
    |[]->[]
    |hd::tl->(reverse tl)@[hd]

let rec dup_remove
= fun lst -> 
    match lst with
        |[]->[]
        |hd::tl->
          if dup_checker hd (dup_remove tl) then dup_remove tl
          else hd::dup_remove tl
 
let rec uniq : 'a list -> 'a list
= fun lst -> 
    match lst with
        |[]->[]
        |hd::tl-> reverse(dup_remove (reverse lst));;
  
uniq[5;6;5;4];;