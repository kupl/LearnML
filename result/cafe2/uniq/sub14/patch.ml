let rec find e (__fun__ : 'a list) : bool =
  match __fun__ with [] -> false | h :: t -> h = e || find e t


let add (e : int) (lst : int list) : 'b list = [ e ] @ lst

let rec uniq (lst : int list) : 'b list =
  match List.rev lst with
  | [] -> lst
  | h :: t -> if find h t then uniq (List.rev t) else uniq (List.rev t) @ [ h ]


let (_ : int list) = uniq [ 5; 6; 5; 4; 3 ]
