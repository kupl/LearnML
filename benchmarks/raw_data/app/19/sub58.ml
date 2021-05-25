
let rec ismember: 'a->'a list->bool
=fun member lst->
  match lst with
    |[]->false
    |hd::tl->if hd=member then true
            else (ismember member tl);;

let rec remove_all_a: 'a->'a list->'a list
=fun member lst->
  match lst with
    |[]->[]
    |hd::tl->if hd=member then (remove_all_a member tl)
            else hd::(remove_all_a member tl);;
      

      
let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
match lst with
  |[]->[]
  |hd::tl->hd::(uniq (remove_all_a hd tl));;
          
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
 uniq(l2@l1);;
 


(*완료*)
