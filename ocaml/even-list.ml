let rec even_list a lst = match lst with
  | h::_::t -> even_list (h::a) t
  | [h] -> even_list (h::a) []
  | _ -> a

let rec deal i a lst = match lst with
  | h::t -> if i mod 2 = 0 then
      deal (i+1) (h::a) t
    else
      deal (i+1) a t
  | _ -> a