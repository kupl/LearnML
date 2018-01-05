exception Error of string
let vocalize: string -> string list list =
  fun input ->
    try (
      let split =
        (
          match String.length input
          with 8 -> 4
            | 7 -> 3
            | _ -> raise (Error "It's not a phone number")
        )
      in
      let rec vocal : string -> string list =
        fun input ->
          (
            if (String.length input)= 0
            then []
            else
              if (String.sub input 0 1) = "0"
              then (vocal (String.sub input 1 ((String.length input)-1)))
              else
                (
                  match (String.sub input 0 1)
                  with "2" -> ["이"]
                    | "3" -> ["삼"]
                    | "4" -> ["사"]
                    | "5" -> ["오"]
                    | "6" -> ["육"]
                    | "7" -> ["칠"]
                    | "8" -> ["팔"]
                    | "9" -> ["구"]
                    | "1" -> (
                        if (String.length input) = 1
                        then ["일"]
                        else []
                      )
                    | "0" -> []
                    | _ -> raise (Error "It's not a phone number.")
                )@(
                  match String.length input
                  with 4 -> ["천"]
                    | 3 -> ["백"]
                    | 2 -> ["십"]
                    | 1 -> []
                    | _ -> raise (Error "It's not a phone number.")
                )@(vocal (String.sub input 1 ((String.length input)-1)))
          )
      in
        [if (vocal (String.sub input 0 split))=[] then ["영"] else (vocal (String.sub input 0 split))]@[if (vocal (String.sub input split 4))=[] then ["영"] else (vocal (String.sub input split 4))]
    )
    with Error s -> raise (Error s)
      | _ -> raise (Error "cake is a lie")
