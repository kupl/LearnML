exception Error of string

let rec convert var_string n = 
  if (String.length var_string == 0) then n
  else
   convert (String.sub var_string 0 (String.length var_string - 1)) 
            n * 10 +  
             (
                match var_string.[(String.length var_string - 1)] with
                  '0' -> 0
                  |'1' -> 1
                  |'2' -> 2
                  |'3' -> 3
                  |'4' -> 4
                  |'5' -> 5
                  |'6' -> 6
                  |'7' -> 7
                  |'8' -> 8
                  |'9' -> 9
                  |_ -> raise (Error "non valid input")
              )
let rec reader var_int n =
  if (var_int == 0) && (n == 1) then ["¿µ"]
  else if (var_int == 0) || (n > 4) then []
  else
    (reader (var_int/10) (n+1))@
      (
        if (n == 1) then
        (
          match (var_int mod 10) with
            1 -> "ÀÏ"::[]
            |2 -> "ÀÌ"::[]
            |3 -> "»ï"::[]
            |4 -> "»ç"::[]
            |5 -> "¿À"::[]
            |6 -> "À°"::[]
            |7 -> "Ä¥"::[]
            |8 -> "ÆÈ"::[]
            |9 -> "±¸"::[]
            |_ -> []
        )
        else
        (
          let digit = 
          (
            match n with
              2 -> "½Ê"
              |3 -> "¹é"
              |4 -> "Ãµ"
              |_ -> raise (Error "invalid operation")
          ) in
          match (var_int mod 10) with
            1 -> digit::[]
            |2 -> "ÀÌ"::digit::[]
            |3 -> "»ï"::digit::[]
            |4 -> "»ç"::digit::[]
            |5 -> "¿À"::digit::[]
            |6 -> "À°"::digit::[]
            |7 -> "Ä¥"::digit::[]
            |8 -> "ÆÈ"::digit::[]
            |9 -> "±¸"::digit::[]
            |_ -> []
        )
      )
    

let vocalize var_string = 
  if (String.length var_string) == 8 then 
    (reader (convert (String.sub var_string 0 4) 0) 1)::(reader (convert (String.sub var_string 4 4) 0) 1)::[]
  else
    (reader (convert (String.sub var_string 0 3) 0) 1)::(reader (convert (String.sub var_string 3 4) 0) 1)::[]

