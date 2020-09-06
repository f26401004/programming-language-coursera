(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn str => (Char.isUpper o String.sub) (str, 0))

val longest_string1 = List.foldl (fn (acc, str) => 
  if (String.size acc) < (String.size str) then str
  else acc) ""

val longest_string2 = List.foldl (fn (acc, str) => 
  if (String.size acc) <= (String.size str) then str
  else acc) ""

fun longest_string_helper f = List.foldl(fn (acc, str) => 
  if f (String.size acc, String.size str) then str
  else acc) ""

val longest_string3 = longest_string_helper (fn (a, b) => a < b)

val longest_string4 = longest_string_helper (fn (a, b) => a <= b)

val longest_capitalized = (longest_string1 o only_capitals)

val rev_string = (String.implode o List.rev o String.explode)

fun first_answer f items = 
  case items of
  [] => raise NoAnswer
  | x::xs => case f x of
    SOME result => result
    | NONE => first_answer f xs

fun all_answers f items =
  let
    fun all_answers_helper acc items =
      case (acc, items) of
      (_, []) => acc
      | (SOME v, x::xs) => (case f x of 
          NONE => NONE
          | SOME result => all_answers_helper (SOME (result @ v)) xs)
      | _ => NONE
  in
    all_answers_helper (SOME []) items
  end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (String.size)

fun count_some_var (str, pattern) = g (fn _ => 0) (fn x =>
  if String.isSubstring str x then 1
  else 0) pattern

fun check_pat patterns = 
  let
    fun filter_string acc pattern = case pattern of
      Variable x => x::acc
      | ConstructorP (_, p) => filter_string acc p
      | TupleP ps => List.foldl(fn (p, accu) => (filter_string [] p) @ accu) [] ps
      | _ => []
    fun duplicate str_list = case str_list of 
      [] => true
      | x::xs => if List.exists (fn item => item = x) xs then false
                  else duplicate xs
  in
    duplicate (filter_string [] patterns)
  end

fun match (value, pattern) = 
  case pattern of
  Wildcard => SOME []
  | UnitP => (case value of Unit => SOME []
                        | _ => NONE)
  | ConstP i => (case value of Const j => if i = j then SOME []
                                          else NONE 
                            | _ => NONE)
  | TupleP tuple => (case value of Tuple tuple_compared => if List.length tuple_compared = List.length tuple
                                            then all_answers match(ListPair.zip(tuple_compared, tuple))
                                            else NONE
                                  | _ => NONE)
  | ConstructorP (str, p) => (case value of Constructor (str_compared, p_compared) => if str = str_compared
                                                                                        then match (p_compared, p)
                                                                                        else NONE
                                          | _ => NONE)
fun first_match value patterns = 
  SOME (first_answer (fn pattern => match (value, pattern)) patterns)
  handle NoAnswer => NONE