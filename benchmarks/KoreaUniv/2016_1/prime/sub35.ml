let rec check = fun n x -> if(n<=1) then false
								else if(x>n/2) then true
								else if((n mod x)=0) then false
								else check n (x+1)

let prime (*재귀 생각을 못해 일반 함수로 바꾸어 풀었습니다.*)
= fun n -> check n 2
