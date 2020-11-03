(*리스트에 해당하는 숫자가 있는지 확인하는 함수*)
let rec is_in = fun l a->
  match l with 
    |[] -> false
    |hd::tl -> if hd=a then true else is_in tl a;;
    
(*app함수*)
let rec app = fun l1 l2->
  match l1 with 
    |[] -> l2
    |hd::tl -> if (is_in l2 hd) then app tl l2 
                else app tl (l2@[hd]) ;;
                
      
(*테스트*)      
app [4;5;6;7] [1;2;3;4];;
app [3;4;5;6] [1;2;3;4;5];;