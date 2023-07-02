type node = Empty | Inner of node * int * node
type path = End | Left of path | Right of path

let rec tree_from_list = 