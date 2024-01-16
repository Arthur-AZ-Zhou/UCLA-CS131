open List

(*Part 1*)
let rec is_element x lst = match lst with
    | [] -> false
    | head :: tail -> head = x || is_element x tail;;

let rec subset a b = match a with
    | [] -> true
    | head :: tail -> 
        if (is_element head b == false) 
            then false
        else 
            subset tail b;;

(*Part 2*)
let equal_sets a b =
    if (subset a b && subset b a == true) 
        then true
    else 
        false;;

(*Part 3*)
let rec set_union a b = match a with 
    | [] -> b 
    | head :: tail -> 
        if (is_element head b == true)
            then set_union tail b
        else 
            set_union tail (head :: b);;

(*Part 4*)
let rec set_all_union a = match a with (*returns a list*)
    | [] -> 
        (* Printf.printf "Empty list\n"; *)
        []
    | [x] -> x    
    | head :: tail -> set_all_union tail@(set_union head (List.hd tail)) ;;

(*Part 5*)
(*It's actually impossible to write this function due to the way how the paradox works. Let's look at set S of all sets that don't contain themselves. If S 
contains itself then it should not be in the set S but if it doesn't contain itself then it should be in the set S but the fact that it is in set S is a 
contradition. Due to this paradox OCaml doesn't allow sets that contains themselves*)

(*Part 6*)
