let rec damcentravi a lst = match lst with
  | [] -> a
  | h::t -> damcentravi (h::(List.rev a)) t

let rec pick_middle b lst = match lst with
  | [] -> 0
  | h::t -> if List.length lst > b then
      pick_middle (b + 2) t
    else
      h
