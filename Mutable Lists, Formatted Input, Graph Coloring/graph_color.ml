(*Solution to PART 3*)

(* Some functions for displaying colourings *)

(* Printing a list using a function for printing items in the list *)
let printlist item_printer l =
   let rec printlist_aux l =
     match l with
     | [] -> Printf.printf "%c" ']'
     | (h::t) -> Printf.printf "%s" ", ";
                 item_printer h;
                 printlist_aux t
   in (Printf.printf "%c" '[';
       match l with
       | (h::t) -> item_printer h; printlist_aux t
       | _ -> printlist_aux l)

(* A function for displaying an integer item *)
let int_printer i = Printf.printf "%d" i

(* A function for displaying a colour (represented by a string) *)
let show_color c = Printf.printf "%s" c

(* A function for displaying a node, colour pair *)
let show_node_and_color (n,c) =
   Printf.printf "(%d," n; show_color c; Printf.printf ")"

(* A function for showing a (complete) colouring *)
let show_coloring l =
   Printf.printf "\nColoring for the graph: "; printlist show_node_and_color l;
   Printf.printf "\n"

(* Solution to Problem 1 *)
(* Problem 1 Specific Code *)

exception Search_Failure

let ask_user printer config =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y")
   then (raise Search_Failure)
   else ()

(*******************************************************************)
(*Solution to Problem 1*)
(*******************************************************************)
(*get_adj : 'a -> ('a * 'b list) list -> 'b list

Pre: takes in a single element of the type of elmenet list
Description: the second argument is a ('a * 'b list) list, then
  macthes each 'a in the list, to find a match for the first 
  argument, if a match is found then it returns the list from the 
  tuple, otherwise if no match is found just returns an empty list.
Post: returns the list that the match case returns

NOTE: basically returns the list of the adjacent nodes, to the 
  node we are trying to color
*)
let rec get_adj node adj =
  match adj with 
    |[] -> []
    |((a,b)::c) -> if (a = node) then b else get_adj node c

(*isMember : 'a -> 'b -> ('a * 'b) list -> bool

Pre: takes in an 2 elements of different types and a list
  of tuples of both those types
Description: checks whether the tuple of (a, b) occurs inside 
  the third argument, and as such returns a bool value, true if
  the occurance is found otherwise returned false.
Post: returns the bool value
*)
let isMember a b myList = List.mem (a, b) myList;;

(*checker : 'a -> 'b list -> ('b * 'a) list -> bool

Pre: takes in a color, the list of just the adjacent elements of the 
  node in the adjacency list, and the list of the colored elements
Description: the funcntion has to return a bool value, depending on
  whether the element can be colored or not, it passes the arguments
  to the isMember function, and if the values is true, then returns 
  false, which means that the colored list did contain an adjacent
  node with the same color we are trying to use as such that is invalid, 
  otherwise keeps going through the rest of the list until we are sure 
  that the color can be used and returns true in that case.
Post: returns the bool value, whether we can use the color for the node
  or not.
*)
let rec checker col adjs colored = 
  match adjs with 
    |[] -> true 
    |(h::t) -> if (isMember h col colored) then false 
               else checker col t colored;;

(*color_graph : int list -> (int * int list) list -> string list -> unit

Pre: takes in a node list, its adjacency list, and the list of 
  possible colors.
Description: tries to color each node and make sure that its adjacent 
  nodes dont have the same color. Goes through the entire nodes list, until
  it's empty, if such this means that all the nodes have been colored,
  otherwise its returns Search_Failure, in which case the nodes can't be
  colored. 
Post: returns type unit.
*)
let color_graph nodes adjacency colors =
   let rec color_graph_aux nodes colored =
      let rec my_aux node remNodes nCols  =
        (match nCols with 
          |[] -> raise Search_Failure
          |(a::b) -> 
              if (checker a (get_adj node adjacency) colored) then
                try color_graph_aux remNodes ((node,a)::colored) 
                  with Search_Failure -> my_aux node remNodes b
              else my_aux node remNodes b) 
      in 
    match nodes with 
      |[] -> ask_user show_coloring colored
      |(h::t) -> my_aux h t colors
    in try (color_graph_aux nodes []) with
      Search_Failure -> Printf.printf "\nNo (more) colourings possible\n"


(* Solution to Problem 2 *)
(* Problem 2 Code *)

let ask_user_cps printer config succ fail =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y") then (fail ()) else (succ ())

(*color_graph_cps : 
    int list -> (int * int list) list -> string list -> unit

Pre: takes in a node list, its adjacency list, and the list of 
  possible colors.
Description: tries to color each node and make sure that its adjacent 
  nodes dont have the same color. Goes through the entire nodes list, until
  it's empty, if such this means that all the nodes have been colored, and
  as such the result is displayed to the user and they have the option to 
  ask for more solutions, and the algorithm produces them depending on 
  whether they are possible or not. Otherwise its returns a failure, 
  this entire process relies on continuations. 
Post: returns type unit.
*)
let color_graph_cps nodes adjacency colors =
  let rec color_graph_aux nodes colored succ fail =
    let rec my_aux node remNodes nCols =
      (match nCols with 
        |[] -> fail()   (*could not color the nodes*)
        |(a::b) -> 
            if (checker a (get_adj node adjacency) colored) then
              color_graph_aux remNodes ((node,a)::colored) (*try to color the node*)
                                          succ 
                                  (fun () -> my_aux node remNodes b) (*catch the fail*)
            else my_aux node remNodes b) 
    in
  match nodes with 
      |[] -> ask_user_cps show_coloring colored succ fail
      |(h::t) -> my_aux h t colors
   in color_graph_aux nodes [] (fun () -> ())
                               (fun () -> Printf.printf "\nNo (more) colourings\n")
(* #use "graph_color.ml";; *)