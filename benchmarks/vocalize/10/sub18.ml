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
  if (var_int == 0) && (n == 1) then ["��"]
  else if (var_int == 0) || (n > 4) then []
  else
    (reader (var_int/10) (n+1))@
      (
        if (n == 1) then
        (
          match (var_int mod 10) with
            1 -> "��"::[]
            |2 -> "��"::[]
            |3 -> "��"::[]
            |4 -> "��"::[]
            |5 -> "��"::[]
            |6 -> "��"::[]
            |7 -> "ĥ"::[]
            |8 -> "��"::[]
            |9 -> "��"::[]
            |_ -> []
        )
        else
        (
          let digit = 
          (
            match n with
              2 -> "��"
              |3 -> "��"
              |4 -> "õ"
              |_ -> raise (Error "invalid operation")
          ) in
          match (var_int mod 10) with
            1 -> digit::[]
            |2 -> "��"::digit::[]
            |3 -> "��"::digit::[]
            |4 -> "��"::digit::[]
            |5 -> "��"::digit::[]
            |6 -> "��"::digit::[]
            |7 -> "ĥ"::digit::[]
            |8 -> "��"::digit::[]
            |9 -> "��"::digit::[]
            |_ -> []
        )
      )
    

let vocalize var_string = 
  if (String.length var_string) == 8 then 
    (reader (convert (String.sub var_string 0 4) 0) 1)::(reader (convert (String.sub var_string 4 4) 0) 1)::[]
  else
    (reader (convert (String.sub var_string 0 3) 0) 1)::(reader (convert (String.sub var_string 3 4) 0) 1)::[]

