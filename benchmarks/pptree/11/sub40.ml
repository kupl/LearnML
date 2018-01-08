type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
	| Poland | Portugal | Italy | Germany | Sweden | England
	| Croatia | Argentina

type tourna =
	| LEAF of team
	| NODE of tourna * tourna

let pptree t =
  let rec pow2 n = if n<=0 then 1 else 2*pow2 (n-1) in
  let width h = pow2 h-1 in
  let rec make t = match t with
    | LEAF _ -> ["|"]
    | NODE(t1,t2) ->
	  let rec stretch l n =
        let original_height = List.length l in
        let original_width = width original_height in
        let new_width = width n in
        let spaces = String.make ((new_width-original_width)/2) ' ' in
        let rec dup l n = if n>0 then dup ((List.hd l)::l) (n-1) else l in
        List.rev (dup (List.rev (List.map (fun x -> spaces^x^spaces) l)) (n-original_height))
      in
      let (s1,s2) = (make t1, make t2) in
      let m = max (List.length s1) (List.length s2) in
      let (s1',s2') = (stretch s1 m, stretch s2 m) in
      (String.make (width m) ' '^"|"^String.make (width m) ' ')::
      (String.make (width (m-1)) ' '^"|"^String.make (width m) '-'^"|"^String.make (width (m-1)) ' ')::
      List.map (fun (a,b) -> a^" "^b) (List.tl (List.combine s1' s2'))
  in
  List.iter print_endline (make t)

