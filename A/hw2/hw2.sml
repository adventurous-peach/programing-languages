(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 
* a' list * a' -> bool
* Check if e is a member of the given list
* is_present([1, 2, 3], 2) = true
* is_present([1, 2, 3], 4) = false
* is_present(["a", "b", "c"], "a") = true
* is_present(["a", "b", "c"], "4") = false
*)
fun is_present(lst, e) =
   case lst of
        [] => false
      | x::xs => x = e orelse is_present(xs, e)

(* put your solutions for problem 1 here *)

(* a' * a' list -> 'a option
* Return an a' option removing e from the given list if e is a member of the list
* Assume e is listed once at most
* all_except_option("a", ["a", "b", "c", "d"]) = SOME(["b", "c", "d"])
* all_except_option("a", ["a", "b", "c"]) = SOME(["b", "c"])
* all_except_option("a", ["a"]) = SOME([])
* all_except_option("d", ["a", "b", "c"]) = NONE
*)
fun all_except_option(e, lst) =
   let 
      fun helper_fun(x, acc) =
         case x of
              [] => acc
            | x'::xs => helper_fun(xs, if x' = e then acc else acc @ [x'])
   in
      if is_present(lst, e) then SOME(helper_fun(lst, [])) else NONE end 

(* a' list list * a' -> 'a list
* ???
* get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Fa", "Ga"]
* get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []
*)
fun get_substitutions1(lst, e) =
   case lst of
        [] => []
      | x::xs => case all_except_option(e, x) of
                   NONE => get_substitutions1(xs, e)
               | SOME v => v @ get_substitutions1(xs, e)

(* a' list list * a' -> a' list
* ???
* get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Fa", "Ga"]
* get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []
*)
fun get_substitutions2(lst, e) =
 let
    fun helper_fun(x, acc) =
       case x of
            [] => acc
          | x'::xs => case all_except_option(e, x') of
                           NONE => helper_fun(xs, acc)
                         | SOME v => helper_fun(xs, acc @ v)
 in helper_fun(lst, []) end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*
* card -> color
* Return the given card's color
* card_color(Spades, Queen) = Black
* card_color(Clubs, Ace) = Black
* card_color(Diamonds, King) = Red
* card_color(Hearts, Num 10) = Red
*)
fun card_color(c) =
   case c of
        (Spades, _) => Black 
      | (Clubs, _) => Black
      | _ => Red

(*
* card -> rank
* Return the given card's value
* card_value(Spades, Queen) = 10
* card_value(Clubs, Ace) = 11
* card_value(Hearts, Jack) = 10
* card_value(Diamonds, King) = 10
* card_value(Spades, Num 10) = 10
* card_value(Clubs, Num 2) = 2
* card_value(Clubs, Num 5) = 5
*)
fun card_value(c) =
   case c of
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

(*
* card list * card * exception -> card list
* Return cs with card c removed or raise exception e if c is not a member of cs
* (remove_card([], (Clubs, King), IllegalMove) handle IllegalMove => (Clubs, King)) = (Clubs, King)
* (remove_card([(Hearts, Ace), (Clubs, King)], (Spades, Num 2), IllegalMove) handle IllegalMove => (Clubs, King)) = (Clubs, King)
* remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
* remove_card([(Hearts, Ace), (Diamonds, King), (Clubs, Num 2)], 
*              (Diamonds, King), IllegalMove) = [(Hearts, Ace), (Clubs, Num 2)]
*)
fun remove_card(cs, c, e) =
   case cs of
        [] => raise e
      | _ => if is_present(cs, c) 
             then case all_except_option(c, cs) of
                       SOME v => v
             else raise e

(* 
* card list -> bool
* Check if all cards, members of the given list, are of the same color
* all_same_color([(Spades, Ace), (Clubs, King)]) = true
* all_same_color([(Hearts, Ace), (Clubs, King)]) = false
* all_same_color([(Spades, Ace), (Diamonds, King)]) = false
* all_same_color([(Hearts, Ace), (Diamonds, King)]) = true
* all_same_color([]) = true
* all_same_color([(Hearts, Num 2)]) = true
* all_same_color([(Hearts, Ace), (Diamonds, King), (Clubs, Num 2)]) = false
*)
fun all_same_color(cs) =
   case cs of
       x::y::z => card_color(x) = card_color(y) andalso all_same_color(y::z)
      | _ => true

(*
* card list -> int 
* Return the value of all the cards in the given list
* sum_cards([(Spades, Ace), (Clubs, King)]) = 21
* sum_cards([(Hearts, Num 2)]) = 2
* sum_cards([(Hearts, Ace), (Diamonds, King), (Clubs, Num 2)]) = 23
* sum_cards([]) = 0
*)
fun sum_cards(c) =
   let
      fun helper_fun(x, acc) =
         case x of
              [] => acc
            | x'::xs => helper_fun(xs, acc + card_value(x'))
   in helper_fun(c, 0) end
