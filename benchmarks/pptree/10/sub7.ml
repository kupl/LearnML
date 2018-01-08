type team =
  | Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
  | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;;

type tourna = LEAF of team | NODE of tourna * tourna;;

let pptree tourna =
  let rec height x =
    match x with
    | LEAF _ -> 0
    | NODE (left, right) -> max (height left) (height right) + 1
  in
  let tourna_height = height tourna in
  let center = (1 lsl tourna_height) - 1 in
  let matrix =
    Array.make_matrix ((1 lsl (tourna_height + 1)) - 1) (tourna_height + 1) ' '
  in
  let rec iter t x y =
    match t with
    | LEAF _ ->
      for i = y to tourna_height do
        matrix.(x).(i) <- '|'
      done
    | NODE (left, right) ->
      let width = ((1 lsl height t) - 2) / 2 in
      for i = x - width to x + width do
        matrix.(i).(y) <- '-'
      done;
      matrix.(x - width - 1).(y) <- '|'; iter left  (x - width - 1) (y + 1);
      matrix.(x + width + 1).(y) <- '|'; iter right (x + width + 1) (y + 1)
  in
  let _ =
    matrix.(center).(0) <- '|'; iter tourna center 1
  in
  for y = 0 to tourna_height do
    for x = 0 to ((1 lsl (tourna_height + 1)) - 2) do
      print_char matrix.(x).(y);
    done;
    print_endline "";
  done;;
