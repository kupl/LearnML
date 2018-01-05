open String;;
exception Invalid_Input

let vocalize source = 
(*
 (* ������ �ڸ��� ��� : int -> int *)
 let rec cipherCheck n = 
  if (n = 0)
  then (0)
  else (1 + (cipherCheck (n/10)))
 in
*)

 (* ������ ���ڿ��� �ش� ������ ���ڿ� ������� ��ȯ : string -> list string *)
 let intToString num_src = 
 
  (* ���ڸ� ���� ���ڸ� �ش� ������ ���ڿ��� ��ȯ�ϴ� ������ ���̺� : char -> string *)
  let digitToString d =
   match d with
   '0' -> "��"
   |'1' -> "��"
   |'2' -> "��"
   |'3' -> "��"
   |'4' -> "��"
   |'5' -> "��"
   |'6' -> "��"
   |'7' -> "ĥ"
   |'8' -> "��"
   |'9' -> "��"
   |_ -> raise Invalid_Input (* �߸��� input *)
  in

  (* ���ڿ��� �� �� ���ڸ� �߶��ִ� �Լ� : string -> string *)
  let cuthead str = (sub str 1 ((length str) - 1 ))
  in

  (* ��ȯ�� ���� �߰� �ܰ� ���ν��� : string -> list string *)
  let rec pre_itos pre_src = 
   let front = (get pre_src 0) in
   let size = (length pre_src) in
   match size with
   1 -> ([digitToString front])
   |2 -> ([(digitToString front); "��"] @ (pre_itos (cuthead pre_src)))
   |3 -> ([(digitToString front); "��"] @ (pre_itos (cuthead pre_src)))
   |4 -> ([(digitToString front); "õ"] @ (pre_itos (cuthead pre_src)))
   |_ -> raise Invalid_Input
  in

  (* pre_itos�� ���� ó�� �� list string ���� �ʿ� ���� �κ��� ���� : list string -> list string *)
  let rec remove_redun target = 
   match target with
    "��"::cipher::lst -> (remove_redun lst)
    |["��"] -> []
    |"��"::cipher::lst -> cipher::(remove_redun lst)
    |digit::cipher::lst -> digit::cipher::(remove_redun lst)
    |[digit] -> [digit]
    |_ -> raise Invalid_Input
  in

 
  (* emptyToZero : string list -> string list - ����ִ� string list �� ["zero"] �� ��ȯ *)
  let emptyToZero target = 
   match target with 
    [] -> ["��"]
    |etc -> etc
  in 
 
 (* �Լ� ���� *)
 (emptyToZero (remove_redun (pre_itos num_src)))
 in
 (* intToString �� *)

 (* ������ ���ڿ��� �̷��� ��ȭ��ȣ�� ���ڸ��� ���ڸ��� �����ش�. : string -> list string *)
 let divider ph = 
  let size = (length ph) in
  match size with
  7 -> [(sub ph 0 3);
        (sub ph 3 4)]
  |8 -> [(sub ph 0 4);
    	 (sub ph 4 4)]
  |_ -> raise Invalid_Input (* input�� size error *)
 in
 (* divider �� *)

 match (divider source) with
  front::rear::lst -> [(intToString front);
 		       (intToString rear)]
  |_ -> raise Invalid_Input
;;
