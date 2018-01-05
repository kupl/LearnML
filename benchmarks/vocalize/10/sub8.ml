exception Error of string

let print1(c) =
        match c with
        '0' -> [] 
        |'1' -> "��"::[]  
        |'2' -> "��"::[] 
        |'3' -> "��"::[] 
        |'4' -> "��"::[] 
        |'5' -> "��"::[] 
        |'6' -> "��"::[] 
        |'7' -> "ĥ"::[] 
        |'8' -> "��"::[] 
        |'9' -> "��"::[] 
        |_ -> []
;;        

let print2(c, st) =
        match c with
        '0' -> [] 
        |'1' -> [st] 
        |'2' -> "��"::[st]
        |'3' -> "��"::[st]
        |'4' -> "��"::[st]
        |'5' -> "��"::[st]
        |'6' -> "��"::[st]
        |'7' -> "ĥ"::[st]
        |'8' -> "��"::[st]
        |'9' -> "��"::[st]
        |_ -> []
;;        

let rec change(str, n) =
        match n with
        1 -> print1(str.[0])
        |2 -> print2(str.[0], "��")@change(String.sub str 1 (n-1), n-1)
        |3 -> print2(str.[0], "��")@change(String.sub str 1 (n-1), n-1)
        |4 -> print2(str.[0], "õ")@change(String.sub str 1 (n-1), n-1)
        |_ -> []
;;

let check(str, n) =
        if int_of_string str = 0 then ["��"]
        else change(str,n)
;;

let vocalize str =
        match String.length str with
        7 -> [check(String.sub str 0 3, 3); check(String.sub str 3 4, 4)]
        |8 -> [check(String.sub str 0 4, 4); check(String.sub str 4 4, 4)]
        |_ -> raise (Error "7 or 8 digits only")
;;
