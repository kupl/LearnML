exception Error of string;;

let vocalize num =
  let num =
    match String.length num with
    | 7 -> "0" ^ num
    | 8 -> num
    | _ -> raise (Error "length <= 6 || length >= 9")
  in
  let read_one digit radix =
    let read_digit x =
      match x with
      | '0' -> "영"
      | '1' -> "일"
      | '2' -> "이"
      | '3' -> "삼"
      | '4' -> "사"
      | '5' -> "오"
      | '6' -> "육"
      | '7' -> "칠"
      | '8' -> "팔"
      | '9' -> "구"
      | _ -> assert false
    and read_radix x =
      match x with
      | 1 -> "십"
      | 2 -> "백"
      | 3 -> "천"
      | _ -> assert false
    in
    match digit with
    | '0' -> []
    | '1' when radix > 0 -> read_radix radix :: []
    | _ when radix > 0 -> read_digit digit :: read_radix radix :: []
    | _ when radix = 0 -> read_digit digit :: []
    | _ -> assert false
  in
  let rec read start stop radix lst =
    if start < stop then
      read (start + 1) stop (radix - 1) (lst @ read_one num.[start] radix)
    else
      if lst = [] then "영" :: [] else lst
  in
  [read 0 4 3 []; read 4 8 3 []];;
