(*Solution to part 1*)

type 'a mylist = 'a listcell ref
    and 'a listcell = Nil | Cons of 'a * ('a mylist);;

(*Solution to Problem 1
append : 'a mylist -> 'a listcell ref -> 'a mylist

Pre: takes in 2 lists of type mylist
Description: goes to the end of the first list and 
    appends the second list to the end of the first
Post: destructively changes the first list, while keeping the 
    second one in tact
*)
let append l1 l2 =
    let rec append' x1 =
        match !x1 with
            |Nil -> x1 := !l2
            |Cons (a, b) -> append' b
    in let nList = l1 in let _ = append' l1 in nList;;

(*Solution to Problem 2
rev_app : 'a mylist -> 'a mylist -> 'a mylist

Pre: takes in 2 lists of type mylist
Description: Reverses the first list in place and then appends the
    second list to it and returns it
Post: returns the list, while destructively changing the first,
    not changing the second one at all. 
*)
let rec rev_app l1 l2 = 
    match !l1 with
        |Nil -> l2
        |Cons(a,b) -> let nval = b in (l1 := Cons(a,l2); rev_app nval l1)