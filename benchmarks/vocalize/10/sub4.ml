(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-3 *)

exception Error of string

let digit c = if c == '0' then "영"
  		else if c == '1' then "일"
  		else if c == '2' then "이"
		else if c == '3' then "삼"
		else if c == '4' then "사"
		else if c == '5' then "오"
		else if c == '6' then "육"
		else if c == '7' then "칠"
		else if c == '8' then "팔"
		else if c == '9' then "구"
		else if c == 'a' then "십"
		else if c == 'b' then "백"
		else if c == 'c' then "천"
		else raise (Error "What I have to say?")

(*
let digit c = if c == '0' then "young"
  		else if c == '1' then "il"
  		else if c == '2' then "ee"
		else if c == '3' then "sam"
		else if c == '4' then "sar"
		else if c == '5' then "oh"
		else if c == '6' then "yook"
		else if c == '7' then "chil"
		else if c == '8' then "pal"
		else if c == '9' then "goo"
		else if c == 'a' then "ship"
		else if c == 'b' then "back"
		else if c == 'c' then "chun"
		else raise (Error "What I have to say?")
*)

let rec voci3 (s, i) = if String.length s != 3 then raise (Error "invalid input in voci3")
			else if i == 3 then []
			else if i == 0 then (
				if (String.get s i) == '0' then voci3 (s, i+1)
				else if (String.get s i) == '1' then (digit 'b') :: voci3 (s, i+1)
				else (digit (String.get s i)) :: (digit 'b') :: voci3 (s, i+1)
				)
			else if i == 1 then (
				if (String.get s i) == '0' then voci3 (s, i+1)
				else if (String.get s i) == '1' then (digit 'a') :: voci3 (s, i+1)
				else (digit (String.get s i)) :: (digit 'a') :: voci3 (s, i+1)
				)
			else if i == 2 then (
				if (String.get s i) == '0' then (
					if (String.get s 0) == '0' && (String.get s 1) == '0' then (digit '0') :: voci3 (s, i+1)
					else voci3 (s, i+1)
					)
				else (digit (String.get s i)) :: voci3 (s, i+1)
				)
			else (digit (String.get s i)) :: voci3 (s, i+1)

let rec voci4 (s, i) = if String.length s != 4 then raise (Error "invalid input in voci4")
			else if i == 4 then []
			else if i == 0 then (
				if (String.get s i) == '0' then voci4 (s, i+1)
				else if (String.get s i) == '1' then (digit 'c') :: voci4 (s, i+1)
				else (digit (String.get s i)) :: (digit 'c') :: voci4 (s, i+1)
				)
			else if i == 1 then (
				if (String.get s i) == '0' then voci4 (s, i+1)
				else if (String.get s i) == '1' then (digit 'b') :: voci4 (s, i+1)
				else (digit (String.get s i)) :: (digit 'b') :: voci4 (s, i+1)
				)
			else if i == 2 then (
				if (String.get s i) == '0' then voci4 (s, i+1)
				else if (String.get s i) == '1' then (digit 'a') :: voci4 (s, i+1)
				else (digit (String.get s i)) :: (digit 'a') :: voci4 (s, i+1)
				)
			else if i == 3 then (
				if (String.get s i) == '0' then (
					if (String.get s 0) == '0' && (String.get s 1) == '0' then (digit '0') :: voci4 (s, i+1)
					else voci4 (s, i+1)
					)
				else (digit (String.get s i)) :: voci4 (s, i+1)
				)
			else (digit (String.get s i)) :: voci4 (s, i+1)

let rec checknum s = if String.length s == 0 then false
		else if String.get s 0 == '0' || String.get s 0 == '1' || String.get s 0 == '2' || String.get s 0 == '3' || String.get s 0 == '4' || String.get s 0 == '5' || String.get s 0 == '6' || String.get s 0 == '7' || String.get s 0 == '8' || String.get s 0 == '9' then checknum (String.sub s 1 (String.length s - 1))
		else true
			
let vocalize s = if checknum s then raise (Error "invalid input in vocalize")
		else if String.length s == 7 then (voci3 (String.sub s 0 3, 0)) :: (voci4 (String.sub s 3 4, 0)) :: []
		else if String.length s == 8 then (voci4 (String.sub s 0 4, 0)) :: (voci4 (String.sub s 4 4, 0)) :: [] 
		else raise (Error "invalid input in vocalize")
(*
(* test code *)

let _ = List.iter (fun x -> print_string x; print_string " ") (List.hd (vocalize "12042451"));
	print_newline();
	List.iter (fun x -> print_string x; print_string " ") (List.hd (List.tl (vocalize "12042451")));
	print_newline();

*)
