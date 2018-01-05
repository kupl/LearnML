
let rec voc s =

  if String.length s = 0 then []
  else

  let s1 = 
  match String.get s 0 with
           '1' -> "일"
	 | '2' -> "이"
	 | '3' -> "삼"
	 | '4' -> "사"
	 | '5' -> "오"
	 | '6' -> "육"
	 | '7' -> "칠"
	 | '8' -> "팔"
	 | '9' -> "구"
	 | '0' -> ""
	 | c -> "" in
  
  let s2 =
  match String.length s with
      1 -> ""
   |  2 -> "십"
   |  3 -> "백"
   |  4 -> "천"
   |  c -> "" in

   List.append (List.append [s1] [s2]) (voc (String.sub s 1 ((String.length s) -1)));;

let vocalize s =

  let l = voc (String.sub s 0 (String.length s - 4)) in
  let r = voc (String.sub s (String.length s - 4) 4) in
  List.append [l] [r]


