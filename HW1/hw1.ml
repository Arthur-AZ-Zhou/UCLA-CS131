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
let rec computed_fixed_point eq f x = (*eq takes in 2 parameters*)
    if eq (f x) x
        then x
    else   
        computed_fixed_point eq f (f x);; (*I don't actually have any way of preventing infinite loops but they said you could do whatever you want so*)

(*Part 7*)
let rec computed_periodic_point_helper f p x = (*iterates so compute_period_point doesn't have to*)
    if (p = 1)
        then (f x)
    else
        computed_periodic_point_helper f (p - 1) (f x);;

let rec computed_periodic_point eq f p x = 
    if (p = 0) 
        then x
    else if (p = 1)
        then computed_fixed_point eq f x
    else if (eq (computed_periodic_point_helper f p x) x) 
        then x
    else 
        computed_periodic_point eq f p (f x);;

(*Part 8*)
let rec whileseq s p x = 
    if p x = false 
        then []
    else 
        x::(whileseq s p (s x));;

(*Part 9*)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let is_terminal_symbol s terminal_list = match s with (*check if symbol is in a terminal, or non-terminal then determined as terminal*)
    | T _ -> true
    | N s -> is_element s terminal_list;;

let rec check_rules r terminal_list = match r with (*true if ALL rules terminal*)
    | [] -> true (*escape condition, we recurse thru whole list*)
    | head :: tail ->
        if (is_terminal_symbol head terminal_list) 
            then check_rules tail terminal_list
        else 
            false;;

let rec create_grammar g terminal_list = match g with (*makes a list of terminals*)
    | [] -> terminal_list
    | (s, r) :: tail -> 
        if (check_rules r terminal_list) && not (is_element s terminal_list)
            then create_grammar tail (s::terminal_list)
        else
            create_grammar tail terminal_list;;

let grammar_wrapper (original_list, terminal_list) = original_list, (create_grammar original_list terminal_list);; (*return object in tuple format*)

let second_element_equal (i1, j1) (i2, j2) = equal_sets j1 j2;;

let rec rule_filter old_rules terminal_list new_rules = match old_rules with
    | [] -> new_rules
    | (s, r) :: tail ->
        if (check_rules r terminal_list)
            then rule_filter tail terminal_list (new_rules @ [(s, r)])
        else
            rule_filter tail terminal_list new_rules;;
            
let filter_blind_alleys g = (fst, g), (rule_filter (snd g) (snd (computed_fixed_point second_element_equal grammar_wrapper ((snd g), []))) []);;