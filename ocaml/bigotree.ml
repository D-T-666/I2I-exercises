type node = Empty | Inner of node * int * node

let rec insertintree v t = match t with
  | Empty -> Inner (Empty, v, Empty)
  | Inner (l, u, r) -> if v > u then 
      Inner (l, u, insertintree v r) 
    else 
      Inner (insertintree v l, u, r)

let rec totree a lst = match lst with
  | [] -> a
  | h::t -> insertintree h (totree a t)

let rec tolist t = match t with
  | Empty -> []
  | Inner (l, v, r) -> tolist l @ [v] @ tolist r

let rec insert n lst = match lst with
  | [] -> [n]
  | h::t -> if n > h then
      h::(insert n t)
    else
      n::h::t

let rec sort lst = match lst with
  | [] -> []
  | h::t -> insert h (sort t)