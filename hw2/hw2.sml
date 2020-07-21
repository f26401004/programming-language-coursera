(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1: string, s2: string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (str, str_list) =
   case str_list of
   [] => NONE
   | s::ss => 
      if same_string(str, s)
      then SOME ss
      else case all_except_option(str, ss) of
         NONE => NONE
         | SOME result => SOME(s::result)
      
fun get_substitutions1(str_list_list, str) =
   case str_list_list of
   [] => []
   | str_list::str_list_rest => 
      case all_except_option(str, str_list) of
      NONE => get_substitutions1(str_list_rest, str)
      | SOME result => result @ get_substitutions1(str_list_rest, str)

fun get_substitutions2(str_list_list, str) =
   let fun iterate(str_list, result) =
      case str_list of
         [] => result
         | list_head::list_rest => 
            case all_except_option(str, list_head) of
            NONE => iterate(list_rest, result)
            | SOME temp => iterate(list_rest, result @ temp)
   in
      iterate(str_list_list, [])
   end

fun similar_names(str_list, {first=first_name, last=last_name, middle=middle_name}) =
   let val first_substitution = get_substitutions2(str_list, first_name)
      fun iterate(first_name_list, result) =
         case first_name_list of
         [] => result
         | list_head::list_rest => iterate(list_rest, {first=list_head, last=last_name, middle=middle_name}::result)
   in
      iterate(first_substitution, [{first=first_name, last=last_name, middle=middle_name}])
   end

fun card_color(card_type, _) = 
   case card_type of Clubs => Black | Spades => Black | Diamonds => Red | Hearts => Red
fun card_value(_, number) =
   case number of
   Ace => 11
   | Num result => result
   | _ => 10

fun remove_card(cards, target_card, custom_exception) =
   case cards of
   [] => raise custom_exception
   | list_head::list_tail =>
      if list_head = target_card
      then list_tail
      else list_head::remove_card(list_tail, target_card, custom_exception)

fun all_same_color(cards) =
   case cards of
   [] => true
   | _::[] => true
   | list_head::(list_head2::list_tail) => card_color(list_head) = card_color(list_head2)
                                 andalso all_same_color(list_head2::list_tail)

fun sum_cards(cards) =
   case cards of
   [] => 0
   | list_head::list_tail => card_value(list_head) + sum_cards(list_tail)

fun score(cards, goal) =
   let val result = sum_cards(cards)
      val original_score = if result > goal
         then 3 * (result - goal)
         else (goal - result)
   in
      if all_same_color(cards)
      then original_score div 2
      else original_score
   end

fun officiate(cards, moves, goal) =
   let fun game_loop(current_cards, held_cards, moves) =
      if sum_cards(held_cards) > goal orelse moves = []
      then score(held_cards, goal)
      else case moves of
      Draw::moves_rest => (case current_cards of
         [] => score(held_cards, goal)
         | card::card_rest => game_loop(card_rest, card::held_cards, moves_rest))
      | (Discard card)::moves_rest => game_loop(current_cards, remove_card(held_cards, card, IllegalMove), moves_rest)
   in
      case cards of
      [] => 0
      | _ => game_loop(cards, [], moves)
   end
