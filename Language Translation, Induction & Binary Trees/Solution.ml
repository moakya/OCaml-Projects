(***********************Solution to Problem 1.1**************************
(*initial list & reversed list*)
************************************************************************)
type state1 = int list * int list;;

(***********************Solution to Problem 1.2**************************
Getters and Setters for the function we are trying to make
************************************************************************)
(*return head of the first list
getHead : 'a list * 'b -> 'a*)
let getHead (l, r) = List.hd l;;

(*return the tail of the second  list
getTail : 'a list * 'b -> 'a list*)
let getTail (l, r) = List.tl l;;

(*returns the first list
getList : 'a * 'b -> 'a*)
let getList (l, r) = l;;

(*returns the reversed list
getRev : 'a * 'b -> 'b*)
let getRev (l, r) = r;;

(*changes the contents of the list r
putRev : ('a * 'b -> 'c) -> 'a * 'b -> 'a * 'c*)
let putRev exp s = 
    let (l, r) = s in (l, exp s);;

(*changes the first list to the the list without its head
putTail : ('a * 'b -> 'c) -> 'a * 'b -> 'c * 'b*)
let putTail exp s =
    let (l, r) = s in (exp s, r);;

(*puts an empty list into the identifier
putEmpty : 'a -> 'b list*)
let putEmpty = fun s -> [];;

(*seq : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c*)
let seq stat1 stat2 = fun s -> (stat2 (stat1 s));;

(*ifstat : ('a -> bool) -> ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b*)
let ifstat exp stat1 stat2 = 
    fun s -> if (exp s) then (stat1 s) else (stat2 s);;

(*whilestat : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a*)
let rec whilestat exp stat =
    fun s -> ifstat exp 
                (seq stat (whilestat exp stat))
                (fun x -> x) s;;

(*returns bool value depenedant on test case so see 
whether a list is empty or not
l_not_emp : 'a list * 'b -> bool*)
let l_not_emp = fun s -> (getList s) <> [];; 

(*puts the head of the first list into the reversed list
h_cons_r : 'a list * 'a list -> 'a list*)
let h_cons_r = fun s -> (getHead s)::(getRev s);;

(***********************Solution to Problem 1.3**************************
The translated function that was asked for
revprog : '_a list * '_b -> '_a list * '_a list
************************************************************************)
let revprog = 
    (seq (putRev putEmpty)  (*change r to an empty list*)
        (whilestat l_not_emp    (*while l is not empty*)
            (seq (putRev h_cons_r)  (*add the head of the l into r*)
                (putTail getTail))));;  (*put the tail of l into l*)

(***********************Solution to Problem 1.4**************************
Function that takes in a list to be reveresed and calls revprog with the
list and an empty one to store the reveresed list into
revlist : '_a list -> '_a list
************************************************************************)

let revlist l = getRev (revprog (l, []));;

(***********************Solution to Problem 2.1**************************
returns a state being passed in as a seq of the state
dostat : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
************************************************************************)
let dostat = fun s -> seq s;;

(***********************Solution to Problem 2.2**************************
Getters and Setters for the function we are trying to make
************************************************************************)
(*2.2a*)
type state2 = int * int * int;;
    (*i, sum, n*)

(*2.2b*)
(*return the value of i
getI : 'a * 'b * 'c -> 'a*)
let getI (i, sum, n) = i;;

(*return the value of sum 
getSum : 'a * 'b * 'c -> 'b*)
let getSum (i, sum, n) = sum;;

(*return the value of n
getN : 'a * 'b * 'c -> 'c*)
let getN (i, sum, n) = n;;

(*Change the value of i to the value being passed in
putI : ('a * 'b * 'c -> 'd) -> 'a * 'b * 'c -> 'd * 'b * 'c*)
let putI exp s = 
    let (i, sum, n) = s in (exp s, sum, n);;

(*Change the value of sum to the value being passed in
putSum : ('a * 'b * 'c -> 'd) -> 'a * 'b * 'c -> 'a * 'd * 'c*)
let putSum exp s = 
    let (i, sum, n) = s in (i, exp s, n);;

(*Change the value of n to the value being passed in
putN : ('a * 'b * 'c -> 'd) -> 'a * 'b * 'c -> 'a * 'b * 'd*)
let putN exp s = 
    let (i, sum, n) = s in (i, sum, exp s);;

(*returns an int 0 into the function that called it
zero : 'a -> int*)
let zero = (fun s -> 0);;

(*return a bool value whether i = n or not 
i_not_n : 'a * 'b * 'a -> bool*)
let i_not_n = (fun s -> (getI s) <> (getN s));;

(*increment i by 1
i_plus_one : int * 'a * 'b -> int*)
let i_plus_one = (fun s -> (getI s) + 1);;

(*add the value of i and sum into sum
sum_plus_i : int * int * 'a -> int*)
let sum_plus_i = (fun s -> (getI s) + (getSum s));;

(*2.2c*)
(*sumup : '_a * '_b * int -> int * int * int
The translated function*)
let sumup = 
    (seq (putI zero)    (*i = 0*)
        (seq (putSum zero)  (*sum = 0*)
            (dostat (putI i_plus_one)   (*do i = i + 1*)
                (seq (putSum sum_plus_i)    (*sum = sum + i*)
                    (whilestat (i_not_n)    (*while i <> n*)
                        (seq (putI i_plus_one)  (*i = i + 1*)
                            (putSum sum_plus_i)))))));; (*sum = sum + i*)

(*Solution to problem 2.3
Takes in an int and calls sumup on it
sumToN : int -> int*)
let sumToN x = getSum (sumup(0, 0, x));;

(*Solution to problem 3.1

math_fib = { n = m; returns f
           { n = m + 1; returns s1
           { n = m + 2; returns f + s1
           
Property: if n = m fib' n m f s = math_fib n =m
          else if n = m + 1 fib' n m f s = math_fib n = m + 1
          else if n = m + 2 fib' n m f s = math _fib n = m + 2
       
Solution to problem 3.2

Base Case: if n = m then
    fib' n m f s = math_fib n =m 
    f = f by the definition of fib'
    
    else if n = m + 1
    fib' n m f s = math_fib n = m + 1
    s1 = s1 by the definition of fib'
    
    else if n = m + 2
    fib' n m f s = math_fib n = m + 2
    f + s1 = f + s1 by the definition of fib'
    
Inductive Hypothesis: Assume this works for all m > 0 & m <= n

Inductive Step:
    wts n = m + 3 fib' n m f s = math_fib n = m + 3
    
    fib' n m f s = math_fib n = m + 2 + 1
    fib' n m f s = math_fib n = m + 2 && n = m + 1
    fib' n m f s = f + s1 + s2
    by the definitions of fib' and math_fib
    
    proven that fib' returns the mth value in f when n = m, returns the sum of the previous 2 values.
    
Solution to problem 3.3

fib passes in an int n into fib' as n and starting off m f and s at 1, to get the fibonacci sequence, the sum of the
previous 2 values is returned and when n = m returns the mth value of the fibonacci sequence. 
Therefore this is true.
*)

(*Solution to problem 4.1a

Property: for all lst1 member of 'a list & for an arbitrary lst2 
    rev lst1 lst 2 = lst1^R + l2

Solution to problem 4.1b

Base Case: let lst1 be an empty list and lst2 be an arbitrary list
    as such,

    []^R + lst2 = lst2
    here []^R = [] by the definition of rev so,

    [] + lst2 = lst2
    [] + lst2 = lst2 by the definition of append so,

    lst2 = lst2 

Inductive Hypothesis: assuming this works for all lst1 element of 'a list
    and for all lst2 element of 'a list
        rev lst1 lst 2 = lst1^R + l2 


Inductive Step: want to show (x::l1)^R + l2 = l1^R + x::l2

    (x::l1)^R + l2 = l1^R + x::l2
    rev (x::l1) l2 = rev l1 (x::l2) by the definition of rev
    so, 
    rev l1 (x::l2) = l1^R + x::l2
    rev l1 (x::l2) = l1^R + x::l2 by the inductive hypothesis
    so,
    l1^R + x::l2 = l1^R + x::l2

    the property holds.

Solution to problem 4.1c

We can clearly see that reverse is calling the function rev using a 
list lst as lst1, and an empty list as lst2.
By the lemma we have proved above we know, 
    reverse l evals to rev l [] so,
    
    rev l [] evals to l1^R + [] by the definition of rev
    l1^R + [] evals to l1^R by the definition of append
    so the original property
        reverse l = l^R
    has been proven and we can see the property clearly holds 

*****************************************************************

Solution to problem 4.2a

Property: for all l1 element of 'a list and an arbitrary l2 
    length (rev l1 l2) = length l1 + length l2

Solution to problem 4.2b

Base Case: let l1 be an empty list containing [] and pick an 
    arbitrary l2, as such,

    length (rev [] l2) = length [] + length l2
    rev [] l2 = l2 by the definition of rev &
        length [] = 0 by the definition of length
    as such,
    length l2 = 0 + length l2
    length l2 = length l2 obviously

Inductive Hypothesis: assuming this works for all l1 element of 'a list
    and for all l2 element of 'a list 
        length (rev l1 l2) = length l1 + length l2

Inductive step:
    want to show 
        length (rev (x::l1) l2) = length l1 + length l2 + 1
        length (rev (x::l1) l2) = length (rev l1 (x::l2)) 
                                by the definition of rev so,

        length (rev l1 (x::l2)) = length l1 + length l2 + 1
        length (rev l1 (x::l2)) = length l1 + length (x::l2) 
                                by the inductive hypothesis so,

        length l1 + length (x::l2) = length l1 + length l2 + 1
        then, length (x::l2) = length l2 + 1 by the definition of length
        so,
        length l1 + length l2 + 1 = length l1 + length l2 + 1

        this is clearly the same so the property holds.
*)

(*Solution to problem 5

Base Case: let t be an Empty tree & pick an arbitrary x as such,
    insert(t x) evaluates to t'
    then for all x' 
        find (t x') evaluates to true as x' = x, and because
            find (Empty x') can never evaluate to true

Inductive Hypothesis:
    for all t element of 'a such that t is of the form 
        Node(i,l,r) & that P(l) and P(r) are true and hold

Inductive Step: 
    assuming that insert(Node(i,l,r) x) evaluates to t'

    want to show that P(t) holds such that 
        if (find Node(i,l,r) y) evaluates to true then 
            find(t' y) evaluates to true

    The first subcase x < i 
        here t' = Node (i, insert l x, r)
            let insert (l x) evaluates to l'

            if y = i then find (Node (i, insert l x, r) y) is true & the
                same as the inductive hypothesis find (t' y)

            if y < i & find (l y) evaluates to true by the inductive hypothesis
                where P(l) holds so we can say that find (l' y) evaluates to 
                true, and by the definition of find we can see that 
                find( Node(i,l',r) y) is the same as find (t' y) and as such
                must also be true

            if y > i then, find(r y) evaluates to true by the definition of 
                find

    the other subcase is when x > i
        here t' = Node (i, l, insert r x)
            let insert (r x) evaluates to r'

            if y = i then find (Node (i, l, insert r x) y) is true & the
                same as the inductive hypothesis find (t' y)

            if y < i then, find(l y) evaluates to true by the definition of 
                find

            if y > i & find (r y) evaluates to true by the inductive hypothesis
                where P(r) holds so we can say that find (r' y) evaluates to 
                true, and by the definition of find we can see that 
                find ( Node(i,l,r') y) is the same as find (t' y) and as such
                must also be true. 

    Hence the property for find holds. QED.
*)
