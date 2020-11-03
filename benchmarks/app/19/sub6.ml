(*
Done.
제가 문제를 해석하는 것으론, 
first list를 second list에 넣으면서, 중복된 것은 제거하는 것으로 이해했습니다.
따라서 app ['a';'b';'d';'b'] ['a';'d';'a';'a'];;의 경우,
second에는 b가 없지만, first에 있는 b를 넣으면서, second에는 b가 있으므로,
first의 마지막 b를 넣을 때는 이미 넣어진 b와 중복 처리하여 넣지 않는 것으로 했습니다.

*)
let rec checkBinA : 'a list -> 'a -> bool 
= fun lst b ->
  match lst with
    | [] -> false
    | hd::tl ->
      if hd = b then true
      else
        checkBinA tl b;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    | [] -> l2
    | hd::tl -> if (checkBinA l2 hd) 
                then 
                  (
                    (app tl l2);
                  )
                else
                  (
                    (app tl (l2@[hd]));
                  );;

app [4;5;6;7] [1;2;3;4];;
app [4;2;6;7] [1;2;3;4];;
app ['a';'b';'d';'b'] ['a';'d';'a';'a'];;
app ['a';'b';'d';'b'] ['a';'b';'c';'d'];;
app ['a';'b';'d';'b'] ['a'];;






