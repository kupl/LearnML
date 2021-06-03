let rec exist el re =
  match re with
    |[]->false
    |hd::tl ->
      if el = hd then true else exist el tl;;

let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec loop remain l = 
    match l with
      |[]->remain
      |hd::tl ->
        if exist hd remain then loop remain tl else loop (remain@[hd]) tl
in loop [] lst;;

uniq [5;6;5;4];;
uniq [1;2;3;4;3;2];;
uniq [1;2;1;2];;