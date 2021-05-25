(*
Done
*)
let rec check_prime (n, idx) =
  if n = 1 || idx = 1 then
    true
  else if (n mod idx) = 0 then
    false
  else
    check_prime (n, idx - 1);;
    
let prime : int -> bool
= fun n -> 
  check_prime(n, n-1);;


prime 1;;
prime 2;;
prime 3;;
prime 4;;
prime 5;;
prime 6;;
prime 7;;
prime 8;;
prime 9;;
prime 10;;
prime 11;;
prime 17;;

(*
문제 정의에 따라 1도 소수라고 하겠습니다.
# - : bool = true   1
# - : bool = true   2
# - : bool = true   3
# - : bool = false  4
# - : bool = true   5
# - : bool = false  6
# - : bool = true   7
# - : bool = false  8
# - : bool = false  9
# - : bool = false  10
# - : bool = true   11
# - : bool = true   17
*)



