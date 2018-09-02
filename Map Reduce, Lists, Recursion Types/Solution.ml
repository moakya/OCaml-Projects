(***********************Solution to Problem 1.1a*************************
makePairLists : 'a -> 'a list -> (('a * 'a) * 'a list) list
Pre: takes in a string and a string list
Description: maps the string over the list using the function inside, 
    where it bascially just combines the element with every element 
    of the string list while maintaining the list
Post: returns the a list of tuples in the tuple, we have another tuple
    of 2 strings and a string list
************************************************************************)
let makePairLists p fl = List.map (fun x -> 
                                        if (p < x) then ((p, x), fl)
                                        else ((x, p), fl)) fl;;

(**********************Solution to Problem 1.1b**************************
makeAllPairLists : ('a * 'a list) list -> (('a * 'a) * 'a list) list list
Pre: takes in a list
Description: maps the list using the function makePairLists where it
    passes a tuple and returns the elements separately
Post: returns a list of tuples, with the tuple containg another tuple of 
    strings and a list
************************************************************************)

let makeAllPairLists l = List.map (fun (x, y) -> makePairLists x y) l;;

(**********************Solution to Problem 1.2a**************************
mutual : 'a -> 'a list -> bool
Pre: takes in a list and an element
Description: checks whether the element is contained in the list if 
    it is return true otherwise false
Post: returns the bool value
************************************************************************)

let rec mutual elm l2 = 
    match l2 with
        | [] -> false
        | h::t -> if (h = elm) then true else mutual elm t;; 

(************************************************************************
val intersect : 'a list -> 'a list -> 'a list
Pre: Takes in 2 lists
Description: checks what the common elements between the two lists are
    then combines those elements those elements into a new list, and 
    maps that onto the original list, otherwise returns the original 
    list.
Post: returns the final list
************************************************************************)

let intersect l1 l2 = List.fold_right (fun x y-> if (mutual x l2) then
                                                    x::y else y)
                                                    l1 [];;

(**********************Solution to Problem 1.2b**************************
addOnePair : 'a * 'b list -> ('a * 'b list) list -> ('a * 'b list) list
Pre: Takes in a tuple of and a list, the list contains a tuple, and the 
    tuple contains a tuple of strings and a list, and the main 
    the tuple being passed in as the arg also conatins a tuple of strings
    and a list 
Description: checks if the tuple of strings matches the tuple 
    of strings inside the list arg, if it matches it takes the
    intersection of both the lists and changes the list to the 
    intersected one, otherwise if a match is not found it places the new
    data at the end of the list arg
Post: returns the final list.
************************************************************************)

let rec addOnePair (fp, ml) lst = 
    match lst with
        |(a, b)::t -> if (a = fp) then (a, intersect ml b)::t
                        else (a, b)::addOnePair (fp, ml) t
        | [] -> (fp, ml)::[];;

(**********************Solution to Problem 1.2c**************************
addAllPairs : ('a * 'b list) list -> ('a * 'b list) list -> 
                    ('a * 'b list) list
Pre: takes in 2 lists of tuples containing a tuple of strings and a list
Description: Calls addOnePair with the elements from the first and 
    the second list to check whether theres anything common, if so
    changes the list to the intersection of the list, otherwise just
    adds it to the end
Post: returns the final list
************************************************************************)

let addAllPairs ppls l = List.fold_left (fun x y-> addOnePair y x) l ppls;;

(***********************Solution to Problem 1.3**************************
commonFriends : ('a * 'a list) list -> (('a * 'a) * 'a list) list
************************************************************************)

let friendsList = [("a", ["b"; "c"; "d"]); ("b", ["a"; "c"; "d"; "e"]);
        ("c", ["a"; "b"; "d"; "e"]); ("d", ["a"; "b"; "c"; "e"]);
        ("e", ["b"; "c"; "d"])];;

let commonFriends l = List.fold_right addAllPairs (makeAllPairLists l) [];;

(***********************Solution to Problem 2.1*************************)
(***********************************************************************)

type 'a olist = { data : 'a list; order : 'a -> 'a -> bool};;

(***********************Solution to Problem 2.2*************************)
(***********************************************************************)

let initOList f = {data = []; order = f};;  

(***********************Solution to Problem 2.3*************************)

let list1 = {data = [1;2;3;4;5]; order = (fun x y -> x < y)};;
let list2 = {data = [5;4;3;2;1]; order = (fun x y -> x > y)};;
let list3 = {data = [5;4;2;1;3]; order = (fun x y -> x > y)};;

(***********************Solution to Problem 2.4**************************
isOrderedList : 'a olist -> bool
Pre: takes a record containing a list and a function inside
Description: using the function specified every element is checked 
    against the other to check whether the order specified by the function
    is followed or not, and a bool value is returned depending on that
Post: return the bool value true if the order is correct else return false
************************************************************************)

let rec isOrderedList {data = lst; order = f} = 
    match lst with 
        | [] -> true 
        | a::b::t -> if (f a b) then true && 
                            (isOrderedList {data = b::t; order = f})
                     else false
        | a::[] -> true;;

(***********************Solution to Problem 2.5**************************
insertOList : 'a -> 'a olist -> 'a olist
Pre: takes in an element and a record of a list of the same type of 
    the element and a function
Description: using the function goes through the list and finds an ideal
    location to insert the element into, editing the list
Post: returns the final list.
************************************************************************)

let insertOList x {data = lst; order = f} =
    let rec insertOlist' x lst f = 
        match lst with 
            | [] -> x::lst
            | a::t -> if (f x a) then x::a::t 
                      else a::(insertOlist' x t f)
    in  {data = insertOlist' x lst f; order = f};;

(***********************Solution to Problem 2.6**************************
olistToList : 'a olist -> 'a list
Pre: Takes in a record containing a list and a function.
Description: goes through the list in the record, and extracts every 
    element from the list and adds it into a new list
Post: returns the final list containing all the elements from the 
    olist
************************************************************************)

let rec olistToList {data = lst; order = f} = 
    match lst with 
        | [] -> []
        | a::t -> a::olistToList{data = t; order = f};;

(***********************Solution to Problem 3.1**************************
cont_append : 'a list -> 'a list -> ('a list -> 'b) -> 'b
Pre: takes in 2 lists
Description: using continuations appends all the elements of the first
    list to the second list, the function in the match takes care of the
    appending portion
Post: returns the final list that holds both lists combined.
************************************************************************)

let rec cont_append l1 l2 f = 
    match l1 with 
        | [] -> (f l2)
        | h::t -> cont_append t l2 (fun x -> f (h::x));;

(***********************Solution to Problem 3.2**************************
cont_sumTree : int btree -> (int -> 'a) -> 'a
Pre: takes in a btree and a function
Description: using the function, walks through the tree and when the end 
    is found returns 0 to the function and then from there gets all 
    the other integer values into the function, then sums them up
    and returns the final reuslt.
Post: returns the sum of the tree 
************************************************************************)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let rec cont_sumTree tree f =
   match tree with
     | Empty -> f 0
     | Node (i, l, r) -> 
        cont_sumTree r (fun x -> cont_sumTree l (fun y -> f (i + y + x)));;