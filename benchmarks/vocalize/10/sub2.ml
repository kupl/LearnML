let vocalize_decimal_value (c) =
match c with
 | '1' -> "일"
 | '2' -> "이"
 | '3' -> "삼"
 | '4' -> "사"
 | '5' -> "오"
 | '6' -> "육"
 | '7' -> "칠"
 | '8' -> "팔"
 | '9' -> "구"
 | _ -> ""
;;

let vocalize_decimal_power (n) = 
match n with
  2 -> "십"
| 3 -> "백"
| 4 -> "천"
| _ -> ""
;;

let vocalize_digit (c, n) = 
if  c != '0'
then
  if n == 1
  then [ vocalize_decimal_value (c) ]
  else
    if c == '1' 
	then [ vocalize_decimal_power (n) ]
	else [ vocalize_decimal_value (c); vocalize_decimal_power (n) ]
else []
;;

let rec vocalize_sub (s) =
if String.length s == 1
then vocalize_digit (String.get s 0, 1)
else vocalize_digit (String.get s 0, String.length s) @ vocalize_sub (String.sub s 1 (String.length s - 1))
;;

let vocalize (s) = 
if String.length s == 7
then [vocalize_sub (String.sub s 0 3); vocalize_sub (String.sub s 3 4)]
else [vocalize_sub (String.sub s 0 4); vocalize_sub (String.sub s 4 4)];;