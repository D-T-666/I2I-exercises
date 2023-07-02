type node = Empty | Inner of node * int * node

let rec insert_in_tree v t = match t with
  | Empty -> Inner (Empty, v, Empty)
  | Inner (l, u, r) -> if v > u then 
      Inner (l, u, insert_in_tree v r) 
    else 
      Inner (insert_in_tree v l, u, r)

let rec to_tree a lst = match lst with
  | [] -> a
  | h::t -> insert_in_tree h (to_tree a t)

let rec to_list t = match t with
  | Empty -> []
  | Inner (l, v, r) -> to_list l @ [v] @ to_list r

let rec insert n lst = match lst with
  | [] -> [n]
  | h::t -> if n > h then
      h::(insert n t)
    else
      n::h::t

let rec sort lst = match lst with
  | [] -> []
  | h::t -> insert h (sort t)