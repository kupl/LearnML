let vocalize_decimal_value (c) =
match c with
 | '1' -> "il"
 | '2' -> "i"
 | '3' -> "sam"
 | '4' -> "sa"
 | '5' -> "o"
 | '6' -> "yuk"
 | '7' -> "chil"
 | '8' -> "pal"
 | '9' -> "gu"
;;

let vocalize_decimal_power (n) = 
match n with
  2 -> "sip"
| 3 -> "baek"
| 4 -> "cheon"
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
;;

let vocalize_sub (s) =
[s];;

let vocalize (s) = 
if String.length s == 7
then [vocalize_sub (String.sub s 0 3); vocalize_sub (String.sub s 3 4)]
else [vocalize_sub (String.sub s 0 4); vocalize_sub (String.sub s 4 4)];;