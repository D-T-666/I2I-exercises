let rec damcentravi a lst = match lst with
  | [] -> a
  | h::t -> damcentravi (h::(List.rev a)) t
  
let rec get_nth n lst = match lst with
  | [] -> 0
  | h::t -> if n = 0 then h else get_nth (n-1) t

let pick_middle lst = get_nth ((List.length lst) / 2) lst;;