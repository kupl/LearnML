exception Error of string

let convert n =
 match n with
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
 |_ -> raise (Error "Invalid argument")


let powerten len =
 match len with
 1 -> []
 |2 -> ["십"]
 |3 -> ["백"]
 |4 -> ["천"]
 |_ -> raise (Error "Invalid argument")


(* make list reverse order *)
let rec vocalize_sub str =
 (let len = (String.length str) in
  if len = 0 then []
  else (let fst = (String.get str 0) in
	match fst with
        '0' -> (vocalize_sub (String.sub str 1 (len-1)))
	|'1' -> (if len = 1 then ["일"] else (powerten len)@(vocalize_sub (String.sub str 1 (len-1))))
        |_ -> ([(convert fst)]@(powerten len)@(vocalize_sub (String.sub str 1 (len-1))))))


let mkzerolst lst =
 if List.length lst = 0 then ["영"] else lst

let vocalize str =
 if (String.length str) = 7 then
	 (let a = String.sub str 0 3 in
	  let b = String.sub str 3 4 in
	  let vocalize_a = mkzerolst (vocalize_sub a) in
	  let vocalize_b = mkzerolst (vocalize_sub b) in
	  [vocalize_a ; vocalize_b]
	  )
 else if (String.length str )= 8 then
	 (let a = String.sub str 0 4 in
	  let b = String.sub str 4 4 in
	  let vocalize_a = mkzerolst (vocalize_sub a) in
	  let vocalize_b = mkzerolst (vocalize_sub b) in
	  [vocalize_a ; vocalize_b]
	 )
 else raise (Error "invalid string")

