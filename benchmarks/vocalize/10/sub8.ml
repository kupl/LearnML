exception Error of string

let print1(c) =
        match c with
        '0' -> [] 
        |'1' -> "ÀÏ"::[]  
        |'2' -> "ÀÌ"::[] 
        |'3' -> "»ï"::[] 
        |'4' -> "»ç"::[] 
        |'5' -> "¿À"::[] 
        |'6' -> "À°"::[] 
        |'7' -> "Ä¥"::[] 
        |'8' -> "ÆÈ"::[] 
        |'9' -> "±¸"::[] 
        |_ -> []
;;        

let print2(c, st) =
        match c with
        '0' -> [] 
        |'1' -> [st] 
        |'2' -> "ÀÌ"::[st]
        |'3' -> "»ï"::[st]
        |'4' -> "»ç"::[st]
        |'5' -> "¿À"::[st]
        |'6' -> "À°"::[st]
        |'7' -> "Ä¥"::[st]
        |'8' -> "ÆÈ"::[st]
        |'9' -> "±¸"::[st]
        |_ -> []
;;        

let rec change(str, n) =
        match n with
        1 -> print1(str.[0])
        |2 -> print2(str.[0], "½Ê")@change(String.sub str 1 (n-1), n-1)
        |3 -> print2(str.[0], "¹é")@change(String.sub str 1 (n-1), n-1)
        |4 -> print2(str.[0], "Ãµ")@change(String.sub str 1 (n-1), n-1)
        |_ -> []
;;

let check(str, n) =
        if int_of_string str = 0 then ["¿µ"]
        else change(str,n)
;;

let vocalize str =
        match String.length str with
        7 -> [check(String.sub str 0 3, 3); check(String.sub str 3 4, 4)]
        |8 -> [check(String.sub str 0 4, 4); check(String.sub str 4 4, 4)]
        |_ -> raise (Error "7 or 8 digits only")
;;
