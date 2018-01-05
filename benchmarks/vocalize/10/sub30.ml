open String;;
exception Invalid_Input

let vocalize source = 
(*
 (* 정수의 자릿수 계산 : int -> int *)
 let rec cipherCheck n = 
  if (n = 0)
  then (0)
  else (1 + (cipherCheck (n/10)))
 in
*)

 (* 숫자의 문자열을 해당 발음의 문자열 목록으로 변환 : string -> list string *)
 let intToString num_src = 
 
  (* 한자리 정수 문자를 해당 발음의 문자열로 변환하는 일종의 테이블 : char -> string *)
  let digitToString d =
   match d with
   '0' -> "영"
   |'1' -> "일"
   |'2' -> "이"
   |'3' -> "삼"
   |'4' -> "사"
   |'5' -> "오"
   |'6' -> "육"
   |'7' -> "칠"
   |'8' -> "팔"
   |'9' -> "구"
   |_ -> raise Invalid_Input (* 잘못된 input *)
  in

  (* 문자열의 맨 앞 문자를 잘라주는 함수 : string -> string *)
  let cuthead str = (sub str 1 ((length str) - 1 ))
  in

  (* 변환을 위한 중간 단계 프로시져 : string -> list string *)
  let rec pre_itos pre_src = 
   let front = (get pre_src 0) in
   let size = (length pre_src) in
   match size with
   1 -> ([digitToString front])
   |2 -> ([(digitToString front); "십"] @ (pre_itos (cuthead pre_src)))
   |3 -> ([(digitToString front); "백"] @ (pre_itos (cuthead pre_src)))
   |4 -> ([(digitToString front); "천"] @ (pre_itos (cuthead pre_src)))
   |_ -> raise Invalid_Input
  in

  (* pre_itos에 의해 처리 된 list string 에서 필요 없는 부분을 제거 : list string -> list string *)
  let rec remove_redun target = 
   match target with
    "영"::cipher::lst -> (remove_redun lst)
    |["영"] -> []
    |"일"::cipher::lst -> cipher::(remove_redun lst)
    |digit::cipher::lst -> digit::cipher::(remove_redun lst)
    |[digit] -> [digit]
    |_ -> raise Invalid_Input
  in

 
  (* emptyToZero : string list -> string list - 비어있는 string list 을 ["zero"] 로 변환 *)
  let emptyToZero target = 
   match target with 
    [] -> ["영"]
    |etc -> etc
  in 
 
 (* 함수 본문 *)
 (emptyToZero (remove_redun (pre_itos num_src)))
 in
 (* intToString 끝 *)

 (* 숫자의 문자열로 이뤄진 전화번호를 앞자리와 뒷자리로 나눠준다. : string -> list string *)
 let divider ph = 
  let size = (length ph) in
  match size with
  7 -> [(sub ph 0 3);
        (sub ph 3 4)]
  |8 -> [(sub ph 0 4);
    	 (sub ph 4 4)]
  |_ -> raise Invalid_Input (* input의 size error *)
 in
 (* divider 끝 *)

 match (divider source) with
  front::rear::lst -> [(intToString front);
 		       (intToString rear)]
  |_ -> raise Invalid_Input
;;
