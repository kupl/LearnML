let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  [] -> []
  |x::xs ->
    let rec isInIt x lst =
      match lst with
        [] -> false
        |y::ys -> if x = y then true else isInIt x ys in
    let rec check checked tocheck =
      match tocheck with
        [] -> []
        |x::xs -> if isInIt x checked 
          then check checked xs
          else x::(check (x::checked) xs)
            in check [] lst;;
              
uniq [2;3;1;5;2;3;4;6;8;5;7];;
