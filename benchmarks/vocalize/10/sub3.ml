type item = string

let reading (num:char) =
  if (compare num 0) = 0 then print_string "��"
  else if (compare num 1) = 0 then print_string "��"
  else if (compare num 2) = 0 then print_string "��"
  else if (compare num 3) = 0 then print_string "��"
  else if (compare num 4) = 0 then print_string "��"
  else if (compare num 5) = 0 then print_string "��"
  else if (compare num 6) = 0 then print_string "��"
  else if (compare num 7) = 0 then print_string "ĥ"
  else if (compare num 8) = 0 then print_string "��"
  else if (compare num 9) = 0 then print_string "��";;

let vocalize item =
  if String.length item = 7 then reading (String.iter (String.get item 0))
  else reading (String.iter (String.get item 0));;

vocalize "88018578"