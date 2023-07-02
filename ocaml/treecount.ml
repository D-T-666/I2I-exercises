type node = Leaf of int | Inner of node * node

let rec countleaves t = match t with
  | Leaf _ -> 1
  | Inner (l, r) -> countleaves l + countleaves r

let rec countinner t = match t with
  | Leaf _ -> 0
  | Inner (l, r) -> 1 + countinner l + countinner r