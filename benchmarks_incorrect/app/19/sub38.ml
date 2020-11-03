let rec insert a l=
  match l with
    |[] ->[a]
    |hd::tl->if a<hd then a::hd::tl
              else hd::(insert a tl)
              
let rec sort l=
  match l with
    |[]->[]
    |hd::tl->insert hd(sort tl)

let rec dup_checker d lst=
   match lst with
     |[]->false
     |hd::tl->
       if hd=d then true
       else dup_checker d tl
 
let rec dup_remove
= fun lst -> 
    match lst with
        |[]->[]
        |hd::tl->
          if dup_checker hd (dup_remove tl) then dup_remove tl
          else hd::dup_remove tl;;
              
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    | []->l2
    | hd::tl->dup_remove(sort(hd::(app tl l2)));;

  
app [4;5;6;7] [1;2;3;4];;
app [4;4;6][1;3;4;5];;
