let vocal2 (int) =
    match int with
      1 -> []
    | 2 -> ["½Ê"]
    | 3 -> ["¹é"]
    | 0 -> ["Ãµ"]
let vocal1 (list,char,int) =
    match char with
      '0' -> if list = [] then ["¿µ"] else []
    | '1' -> if int=1 then ["ÀÏ"] else vocal2(int)
    | '2' -> ["ÀÌ"] @ vocal2(int)
    | '3' -> ["»ï"] @ vocal2(int)
    | '4' -> ["»ç"] @ vocal2(int)
    | '5' -> ["¿À"] @ vocal2(int)
    | '6' -> ["À°"] @ vocal2(int)
    | '7' -> ["Ä¥"] @ vocal2(int)
    | '8' -> ["ÆÈ"] @ vocal2(int)
    | '9' -> ["±¸"] @ vocal2(int)
let rec vocal (list,num,string) =
    let int = (String.length(string) - num) mod 4 in
    match int with
      1 -> if (num+1) = String.length string then [list @ vocal1(list,string.[num], int)]
           else [list @ vocal1(list,string.[num], int)] @ vocal([], num+1, string)
    | _ -> vocal(list @ vocal1(["a"], string.[num], int), num+1, string)
let vocalize string = vocal([],0,string)