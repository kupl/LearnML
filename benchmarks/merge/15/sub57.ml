let rec merge (x, y)=
  match (x, y) with
  | (xhead :: xtail, yhead :: ytail) -> if (xhead < yhead) then (yhead :: xhead :: (merge (xtail, ytail)))
					else if (xhead > yhead) then (xhead :: yhead :: (merge (xtail, ytail)))
					else (xhead :: (merge (xtail, ytail)))
  | (xhead :: xtail, []) -> (xhead :: (merge (xtail, [])))
  | ([], yhead :: ytail) -> (yhead :: (merge ([], ytail)))
  | ([], []) -> []
