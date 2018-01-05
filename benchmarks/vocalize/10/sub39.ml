exception Error of string

let t = [('1',"일");('2',"이");('3',"삼");('4',"사");('5',"오");('6',"육");('7',"칠");('8',"팔");('9',"구");('0',"영")];;

let c1 = [(0,"백");(1,"십");(3,"천");(4,"백");(5,"십");(2,"blank");(6,"blank")];;

let c2 = [(0,"천");(1,"백");(2,"십");(3,"blank");(4,"천");(5,"백");(6,"십");(7,"blank")];;


let first pair =
	match pair with
	(a,b) -> a;;

let last pair =
	match pair with
	(a,b) -> b;;

let f x pair =	
	(first pair) = x
	
let g x =
	match x with
	"영"  -> false
	| _ -> true;;


let case str =		(* 7자리인지 8자리인지 판단 *)
	if (String.length str) = 7 then c1 else c2;; 


let rec get_num_str s n = 	(* 문자열의 n번째 '숫자' 로부터 "한글" 얻는 함수 *)
	last (List.hd (List.filter (f (String.get s n)) t));;


let rec get_10_str s n =	(* 문자열의 n번째 '숫자'의 "자릿수" 얻는 함수 *)
	last (List.hd (List.filter (f n) (case s)));;

let rec paste_str s n =
	if (get_num_str s n) = "영" then []
	else if (get_10_str s n) = "blank" then [get_num_str s n]
	else if (get_num_str s n) = "일" && (get_10_str s n) != "blank" then [get_10_str s n]
	else [(get_num_str s n) ; (get_10_str s n)];;


let rec sub_vocalize s n k=	(* 부분적인 숫자스트링 리스트 만들기 *)
	if (k = 0) then []
	else List.append (paste_str s n) (sub_vocalize s (n+1) (k-1)) ;;
	

let rec vocalize s =
	let n = String.length s in
	if (n<=6 || n>=9) then raise (Error "error")
	else [(sub_vocalize s 0 (n/2)) ; (sub_vocalize s (n/2) (n - (n/2)))] ;;


