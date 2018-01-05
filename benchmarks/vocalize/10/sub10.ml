let rec voc s =

  if String.length s = 0 then []
  else

  let len = String.length s in
  let num = int_of_string (String.sub s 0 1) in

  let s1 = match num with
           1 -> "일"
	 | 2 -> "이"
	 | 3 -> "삼"
	 | 4 -> "사"
	 | 5 -> "오"
	 | 6 -> "육"
	 | 7 -> "칠"
	 | 8 -> "팔"
	 | 9 -> "구"
	 | 0 -> ""
	 | c -> "" in
  
  let s2 =
  match len with
      1 -> ""
   |  2 -> "십"
   |  3 -> "백"
   |  4 -> "천"
   |  c -> "" in

  if (num = 1) & (not (len = 1))
  then List.append [s2] (voc (String.sub s 1 (len -1)))
  else if (num = 0) 
  then List.append [""] (voc (String.sub s 1 (len -1)))
  else List.append (List.append [s1] [s2]) (voc (String.sub s 1 (len -1)));;

let vocalize s =

  let l = voc (String.sub s 0 (String.length s - 4)) in
  let r = voc (String.sub s (String.length s - 4) 4) in
  List.append [l] [r]


