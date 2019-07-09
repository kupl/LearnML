let rec times10 : int list -> int list
= fun lst ->
  match lst with
    | [] -> []
    | hd::tl -> 10*hd :: (times10 tl);;
    
(*let rec find_last : int list -> int
= fun lst ->
  match lst with
    | []->[]
    | [n] -> n
    | hd::tl -> find_last tl;;
역순만드는게 더 좋을듯?*)

let rec reverse : int list -> int list
= fun lst ->
  match lst with
    | [] -> []
    | hd::tl -> (reverse tl) @ [hd];;
    
let rec inting : int list -> int     
= fun lst ->
  match lst with
    | [] -> 0
    | hd::tl -> hd + inting(times10(tl));; 
 (*reverse를 lst2int에 넣어버리면 재귀 과정에서 여러번 역순정렬 되므로 역순이 한번만 진행되게 하기 위해 
 lst2int의 rec을 없애고 이 재귀함수를 만듦*)

let lst2int : int list -> int
= fun lst -> 
  inting (reverse lst);;

lst2int [2;3;4;5];;
lst2int [3;5;4;8;9;5;1;0;2;2];;
