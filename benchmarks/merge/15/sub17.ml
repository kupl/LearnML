let rec append (x,y) : int list = 
  match x with 
    [] -> y
  | h::t -> h :: append(t, y)

let rec bubble x : int list =
  match x with
    [] -> []
  | h::[] -> x
  | h::(i::t) ->
	if(h > i) then h::(bubble(i::t))
	else i::(bubble(h::t))

let rec rev x : int list =
  match x with
    []->[]
  | h::t -> rev t @ [h]

let tl x =
  match x with
    []-> raise (Failure "tl")
  | h::t -> t

let hd x =
  match x with
    []-> raise (Failure "hd")
  | h::t -> h

let rec sort x : int list =
  match x with
    [] -> []
  | a::[] -> x
  | h::t -> hd(rev(bubble x)) :: (sort(tl(rev(bubble x))))


let merge (x,y) : int list = rev(sort(append(x,y)))
