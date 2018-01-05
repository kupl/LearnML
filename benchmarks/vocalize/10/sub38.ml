exception Error of string

let t = [('1',"��");('2',"��");('3',"��");('4',"��");('5',"��");('6',"��");('7',"ĥ");('8',"��");('9',"��");('0',"��")];;

let c1 = [(0,"��");(1,"��");(3,"õ");(4,"��");(5,"��");(2,"blank");(6,"blank")];;

let c2 = [(0,"õ");(1,"��");(2,"��");(3,"blank");(4,"õ");(5,"��");(6,"��");(7,"blank")];;


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
	"��"  -> false
	| _ -> true;;


let case str =		(* 7�ڸ����� 8�ڸ����� �Ǵ� *)
	if (String.length str) = 7 then c1 else c2;; 


let rec get_num_str s n = 	(* ���ڿ��� n��° '����' �κ��� "�ѱ�" ��� �Լ� *)
	last (List.hd (List.filter (f (String.get s n)) t));;


let rec get_10_str s n =	(* ���ڿ��� n��° '����'�� "�ڸ���" ��� �Լ� *)
	last (List.hd (List.filter (f n) (case s)));;

let rec paste_str s n =
	if (get_num_str s n) = "��" then []
	else if (get_10_str s n) = "blank" then [get_num_str s n]
	else if (get_num_str s n) = "��" && (get_10_str s n) != "blank" then [get_10_str s n]
	else [(get_num_str s n) ; (get_10_str s n)];;


let rec sub_vocalize s n k=	(* �κ����� ���ڽ�Ʈ�� ����Ʈ ����� *)
	if (k = 0) then []
	else List.append (paste_str s n) (sub_vocalize s (n+1) (k-1)) ;;
	

let rec vocalize s =
	let n = String.length s in
	if (n<=6 || n>=9) then raise (Error "error")
	else [(sub_vocalize s 0 (n/2)) ; (sub_vocalize s (n/2) (n - (n/2)))] ;;


