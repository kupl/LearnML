let rec search 
= fun hd tl ->
  match tl with
    | [] -> []
    | hd'::tl' -> if hd' = hd then (search hd tl') else hd'::(search hd tl')
              

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  match lst with
    | [] -> []
    | hd::tl -> hd::( uniq (search hd tl) )
;;
(*이방법은 n^3이므로 n^2으로 바꿀 방법을 찾아야함*)

uniq [5;6;5;4];;
uniq [5;3;5;3;5;3;5;3;5;3;5;3;6;7;8;7;6;8;7;6;9];;