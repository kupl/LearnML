(*recursive_checker을 따로 빼서 만들수있지않나*)


let rec prime : int -> bool
= fun n -> (*TODO*)
 let rec recursive_checker m d = 
   match d with
        | 1 -> true    
        |_ -> (m mod d <> 0) && (*d는 m의 약수가 아니고*)
              recursive_checker m (d-1) (*다음 작은 수도 약수인지 여부 체크*)
    in 
    match n with
    | 1 -> false(*1은 소수 검사에서 제외*)
    |_ -> recursive_checker n (n-1) ;;(*해당 수보다 하나 작은 수부터 나누어서 검사*)
  
