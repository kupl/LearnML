(* ÄÄÇ»ÅÍ°øÇÐºÎ / 2005-11721 / ±èÀç°æ / ¼÷Á¦1-3 *)
let vocal2 (int) =
    match int with
      1 -> []
    | 2 -> ["½Ê"]
    | 3 -> ["¹é"]
    | 0 -> ["Ãµ"]
    | _ -> []
let vocal1 (char,int) =
    match char with
      '0' -> []
    | '1' -> if int=1 then ["ÀÏ"] else vocal2(int)
    | '2' -> ["ÀÌ"] @ vocal2(int)
    | '3' -> ["»ï"] @ vocal2(int)
    | '4' -> ["»ç"] @ vocal2(int)
    | '5' -> ["¿À"] @ vocal2(int)
    | '6' -> ["À°"] @ vocal2(int)
    | '7' -> ["Ä¥"] @ vocal2(int)
    | '8' -> ["ÆÈ"] @ vocal2(int)
    | '9' -> ["±¸"] @ vocal2(int)
    |  _  -> []
let rec vocal (list,num,string) =
    let int = (String.length(string) - num) mod 4 in
    let new_list = list @ vocal1(string.[num], int) in
    match int with
      1 -> let new_list1 = 
             if new_list = [] then ["¿µ"] else new_list in
           if (num+1) = String.length string then [new_list1]
           else [new_list1] @ vocal([], num+1, string)
    | _ -> vocal(new_list, num+1, string)
let vocalize string = vocal([],0,string)