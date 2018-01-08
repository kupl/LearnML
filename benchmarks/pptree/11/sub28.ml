type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina | A | B | C | D
type tourna = LEAF of team 
| NODE of tourna * tourna | NONE

exception Error

(* 트리에서 leaf의 개수 *)
let rec num_of_leaf t = 
  match t with
  | LEAF(_) -> 1
  | NODE(a,b) -> num_of_leaf(a) + num_of_leaf(b)
  | NONE -> raise Error

(* 트리의 자식둘중 하나만 leaf 이면 1, 아니면 0 *)
let one_leaf t = 
  match t with
  | LEAF(_) -> 0
  | NODE(a, b) -> (
    match (a, b) with
    | (LEAF(_), NODE(_, _)) -> 1
    | (NODE(_, _), LEAF(_)) -> 1
    | _ -> 0
  )
    | NONE -> raise Error

let rec num_of_one_leaf t = 
  match t with
  | LEAF(_) -> 0
  | NODE(a, b) -> one_leaf(a) + one_leaf(b) + 
  num_of_one_leaf(a) + num_of_one_leaf(b)
  | NONE -> raise Error


(* 어떤 노드의 x 좌표가 몇인지 *)
let rec pos(tree, root) = 
  (* leaf node가 왼쪽에서 x 좌표가 몇인가 *)
  let rec whereami(leaf, current, number) = 
    match current with
    | LEAF(_) -> (
      if leaf == current then
        number
      else
        0
    )
    | NODE(a, b) -> (
      whereami(leaf, a, number) + 
      whereami(leaf, b, number + 2*num_of_leaf(a) + one_leaf(current))
    )
    | NONE -> raise Error
  in

  match tree with
  | NODE(a, b) -> (pos(a, root) + pos(b, root)) / 2
  | LEAF(_) -> whereami(tree, root, 0) (* LEAF 노드의 위치가 왼쪽에서 몇 번째인지 찾는다. *)
  | NONE -> raise Error

let max(a, b) = 
  if a > b
  then a
  else b

(* 트리의 깊이 최대값을 구함 *)
let max_depth t =
  let rec max_depth_p(current, depth) = 
    match current with
    | LEAF(_) -> depth
    | NODE(a, b) -> max( max_depth_p(a, depth+1), max_depth_p(b, depth+1))
    | NONE -> raise Error
  in
  max_depth_p(t, 1)

(* 트리의 x좌표 최대값을 구함 *)
let max_pos root = 
  let rec max_pos_p t =
    match t with
    | LEAF(_) -> pos(t, root)
    | NODE(_, b) -> max_pos_p(b)
    | NONE -> raise Error
  in
  max_pos_p root

(* 좌표가 트리에 존재하는 좌표이면 true, 아니면 false *)
let exist_pos(tree, i, j) =
  let rec exist_pos_p(t, depth) =
    match t with
    | LEAF(_) -> ((pos(t, tree) = j) && (depth = i))
    | NODE(a, b) -> (((pos(t, tree) = j) && (depth = i)) 
                   || exist_pos_p(a, depth+1) || exist_pos_p(b, depth+1))
    | NONE -> raise Error
  in
  exist_pos_p(tree, 1)

(* 부모를 얻음 / 부모가 없으면 LEAF 리턴 *)
let get_parent(t, root) = 
  let rec get_parent_p current =  (* current 의 자식이 t이면 current 리턴 *)
    match current with
    | NODE(a,b) -> (
      if (a == t) || (b == t) then
        current
      else (
        match (get_parent_p(a)) with
        | NODE(_, _) -> get_parent_p(a)
        | _ -> get_parent_p(b)
      )
    )
    | LEAF(_) -> NONE
    | NONE -> raise Error
  in
  get_parent_p root

(* 주어진 좌표를 갖는 트리를 탐색함 *)
let find_tree_with_pos(root, i, j) = 
  let rec find_tree_with_pos_p(current, depth) = 
    match current with
    | LEAF(_) -> (
        if (pos(current, root) = j) && (depth = i) then
          current
        else
          NONE
    )
    | NODE(a, b) -> (
      if (pos(current, root) = j) && (depth = i) then
        current
      else
        match find_tree_with_pos_p(a, depth+1) with
        | NONE -> find_tree_with_pos_p(b, depth+1)
        | _ -> find_tree_with_pos_p(a, depth+1)
    )
    | NONE -> raise Error
  in
  find_tree_with_pos_p(root, 1)

(* 좌표보다 왼쪽의 가장 가까운 트리를 얻음 *)
let get_left_tree(root, i, j) = 
  let result = ref NONE in
  let flag = ref false in
  for k = 0 to j-1 do
    match find_tree_with_pos(root, i, k) with
    | NONE -> print_string "";
    | _ -> (
      if (!flag) = false then (
        result := find_tree_with_pos(root, i, k);
        (* flag := true; *)
      ) 
      else (
        print_string "";
      );
      print_string "";
    )
  done;
  !result

(* 좌표보다 오른쪽의 가장 가가운 트리를 얻음 *)
let get_right_tree(root, i, j) = 
  let result = ref NONE in
  let flag = ref false in
  for k = j+1 to max_pos(root)+1 do
    match find_tree_with_pos(root, i, k) with
    | NONE -> print_string "";
    | _ -> (
      if (!flag) = false then (
        result := find_tree_with_pos(root, i, k);
        flag := true;
      ) 
      else (
        print_string "";
      );
      print_string "";
    )
  done;
  !result

(* 좌표보다 위쪽의 가장 가까운 트리를 얻음 *)
let get_up_tree(root, i, j) = 
  let result = ref NONE in
  let flag = ref false in
  for k = 0 to i-1 do
    match find_tree_with_pos(root, k, j) with
    | NONE -> print_string "";
    | _ -> (
      if (!flag) = false then (
        result := find_tree_with_pos(root, k ,j);
        (* flag := true; *)
      ) 
      else (
        print_string "";
      );
      print_string "";
    )
  done;
  !result


let is_dash(tour, left, right) = 
  match (left, right) with
  | (_, NONE) -> false
  | (NONE, _) -> false
  | _ ->(
    if (get_parent(left, tour) == get_parent(right, tour)) then
      true
    else 
      false
  )


let rec pptree tour = 
  begin
  for i = 1 to (max_depth tour) do
    for j = 0 to (max_pos tour) do
      if exist_pos(tour, i, j) then
        (* 현재 위치가 노드가 있는 위치이면 | 출력 *)
        print_string("|")
      else
        (* 노드가 없는 위치일 경우 '-' 인지 ' ' 인지를 결정해야함 *)
        let left = get_left_tree(tour, i, j) in
        let right = get_right_tree(tour, i, j) in
        let up = get_up_tree(tour, i, j) in
        let dash = is_dash(tour, left, right) in (* - 출력이면 true 아님 false *)

        if dash then
          print_string("-")
        else(
          match up with
          | LEAF(_) -> print_string("|")
          | _ -> print_string(" ")
        )
        
    done;
    print_string("\n");
  done;
  end


let a = LEAF A
let b = LEAF B
let c = LEAF C
let d = LEAF D
let e = LEAF A

let x = NODE(a,b)
let y = NODE(c,d)

let t = NODE(x,y)
let p = NODE(t, e)

