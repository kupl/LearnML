(*리스트 안에 주어진 숫자를 포함하고 있는지 확인하는 함수*)
let rec is_in = fun l a->
  match l with 
    |[] -> false
    |hd::tl -> if hd=a then true else is_in tl a;;
    
(*uniq 함수*)
let rec uniq_ = fun l a->
  match l with
    |[]->a
    |hd::tl -> if (is_in a hd) then uniq_ tl a
               else uniq_ tl (a@[hd]);;
               
let uniq l = uniq_ l [];;

(*테스트*)
uniq [5;6;5;4];;
uniq [2;4;2;5;4;3;5;7];;