exception Error of string
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina
type tourna = LEAF of team
  | NODE of tourna * tourna
let pptree : tourna -> unit =
  fun tourna ->
    let rec repeat : string -> int -> string =
      fun s -> fun i -> if i =0 then "" else s^(repeat s (i-1))
    in
    let merge : string list -> string list -> string list =
      fun sl1 -> fun sl2 ->
        let rec copy : string -> int -> string =
          fun s -> fun i ->
            if (String.length s) == i
            then ""
            else if (String.get s i) == '-'
            then " "^(copy s (i+1))
            else (Char.escaped (String.get s i))^(copy s (i+1))
        in
        let rec merge : string list -> string list -> string list =
          fun sl1 -> fun sl2 ->
            match (sl1, sl2)
            with ([], []) -> []
              | (h1::t1, []) -> (h1^(repeat " " (((String.length h1)/2)+1))^"|"^(repeat " " ((String.length h1)/2)))::(merge t1 [])
              | ([], h2::t2) -> (((repeat " " ((String.length h2)/2)))^"|"^(repeat " " (((String.length h2)/2)+1))^h2)::(merge [] t2)
              | (h1::[], h2::[]) ->
                  let sub1 =
                    match compare (String.length h2) (String.length h1)
                    with  1 -> (String.length h2) - (String.length h1)
                      | _ -> 0
                  in
                  let sub2 =
                    match compare (String.length h1) (String.length h2)
                    with  1 -> (String.length h1) - (String.length h2)
                      | _ -> 0
                  in
                    ((repeat " " (sub1/2))^h1^(repeat " " (sub1/2))^" "^(repeat " " (sub2/2))^h2^(repeat " " (sub2/2)))::(merge [] [])
              | (h1::t1, h2::[]) -> (merge sl1 (h2::[copy h2 0]))
              | (h1::[], h2::t2) -> (merge (h1::[copy h1 0]) sl2)
              | (h1::t1, h2::t2) ->
                  let sub1 =
                    match compare (String.length h2) (String.length h1)
                    with  1 -> (String.length h2) - (String.length h1)
                      | _ -> 0
                  in
                  let sub2 =
                    match compare (String.length h1) (String.length h2)
                    with  1 -> (String.length h1) - (String.length h2)
                      | _ -> 0
                  in
                    ((repeat " " (sub1/2))^h1^(repeat " " (sub1/2))^" "^(repeat " " (sub2/2))^h2^(repeat " " (sub2/2)))::(merge t1 t2)
        in
        let rec change : string -> int -> bool -> unit =
          fun s -> fun i -> fun b -> match String.get s i
          with '|' -> if b then () else change s (i+1) true
            | _ ->
                let _ = (change s (i+1) b) in
                  if b then (String.set s i '-') else ()
        in
          match merge sl1 sl2
          with [] -> []
            | h::t ->
                let _ = (change h 0 false) in h::t
    in
    let rec pptree : tourna -> string list =
      fun tourna -> match tourna
      with LEAF(_) -> ["|"]
        | NODE(n1,n2) ->
            let x = merge (pptree n1) (pptree n2) in
              ((repeat " " (((String.length (List.hd x))/2)))^"|"^(repeat " " ((String.length (List.hd x))/2)))::x

    in
    let rec countmin: string list-> int -> int -> int =
      fun sl -> fun cnt -> fun m ->
        match sl
        with [] -> m
          | sh::st -> (
              if String.get sh cnt = ' '
              then countmin sl (cnt+1) m
              else countmin st 0 (min m cnt)
            )
    in
    let rec countmax: string-> int -> int =
      fun s -> fun cnt ->
              if String.get s cnt = ' ' then countmax s (cnt-1) else cnt
    in
    let rec print : string list -> int -> unit =
      fun sl -> fun start -> match sl
      with [] -> ()
        | h::t ->
            let _ = print_string ((String.sub h start ((countmax h ((String.length h)-1))-start+1))^"\n") in
              print t start
    in
    let x = (pptree tourna) in
    let min = (countmin x 0 ((String.length (List.hd x))-1)) in
      print x min 
