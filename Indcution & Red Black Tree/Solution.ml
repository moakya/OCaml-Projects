(*Solution to Problem 1
*****************************************************************************
Part 1.a

c1 is the cost of return
c2 is the cost of recursion and return

        { c1    n = 0
T(n) =  {
        { T(n-2) + c2  n > 0

*****************************************************************************
Part 1.b

T(0) = c1

T(2) = T(2-2) + c2  
     = c1 + c2 
     
T(4) = T(4-2) + c2 
     = c1 + c2 + c2
     = c1 + 2c2

T(6) = T(6-2) + c2
     = c1 + 2c2 + c2
     = c1 + 3c2

T(8) = T(8-2) + c2
     = c1 + 3c2 + c2
     = c1 + 4c2

T(n) = c1 + (n/2)c2

*****************************************************************************
Part 1.c

Base Case:
T(0) should be c1
so, 
T(0) = c1 + (0/2)c2 
     = c1 + 0c2
     = c1

Inductive Hypothesis: Assume this works for all n >= 0, n being a 
    multiple of 2 i.e.
    T(n) = c1 + (n/2)c2

Inductive Step:
    Want to show that T(n+2) = T(n) + c2

So, 
    T(n+2) = T(n) + c2
    c1 + ((n+2)/2)c2 = T(n) + c2
    c1 + (n/2)c2 + (2/2)c2 = T(n) + c2
    c1 + (n/2)c2 + c2 = T(n) + c2
    ^^^^^^^^^^^^ = T(n) by the Inductive Hypothesis
then, 
    T(n) + c2 = T(n) + c2

QED

*****************************************************************************
Part 2.a

The worst case for merge is when the most amount of comparisons are
made, for a list of size n, because every element in the first list is 
compared against the subsequent element in the second list.

*****************************************************************************
Part 2.b

n1 being the length of the first list and n2 being the length of the second, 
we can look at the if statement 

        if (h1 < h2) then h1 :: merge t1 l2
        else h2 :: merge l1 t2

and clearly see that comparisons are made of the head of each of the lists.
As such all the cons and merge operations will be n1 + n2, where 
every element of the first list is less than the counterpart in the
second list. 
Thus we can deduce that the time taken for merge on two sufficiently large
lists will be some c * (n1 + n2)

*****************************************************************************
Part 3.a

c1 be the cost of return
c2 be the cost of return and recursion
P(n) be the cost of partition
M(n) be the cost of merge


        { c1    n = 0
S(n) <  {
        { S(n/2) + S(n/2) + P(n) + M(n) + c2       n > 0

Mergesort takes in a list, and calls multiple functions which have already been
proven above. The function partition is run only once during each recursive call
and so is merge. Partition splits the list into 2 parts, and returns them as a 
tuple, after which mergesort is called on each part recursively, and in the 
end the parts are appended together using the function merge. That is why 
I said that S(n) for the mergesort is the sum of the mergesort on a sum of 
2 halves of the list, and the sum of the partition and merge functions and 
finally the costs of the recursive calls and return.

*****************************************************************************
Part 3.b

S(n) = n log n + c2

*****************************************************************************
Part 3.c

Base case: S(0) should be c2

S(0) = 0 log(0) + c2 ; log (0) goes to infinity but we can assume it as 0
     = 0(0) + c2 
     = c2

Inductive Hypothesis: We can assume that the property holds for all values 
    upto and including n

Inductive Hypothesis: 
    Want to show S(n+2) = S(n) + log(n)
    
    S(n+2) = (n+1) log(n+1) + c2
           = (n+1) (log(n) + log(1)) + c2
           = n log (n) + n log (1) + log(n) + log(1) + c2
           = n log (n) + n (0) + log(n) + 0 + c2
           = n log (n) + c2 + log(n)
             ^^^^^^^^^^^^^^ = S(n) by the Inductive Hypothesis
           = S(n) + log(n)

    QED

*****************************************************************************
Part 3.d

As I have assumed S(n) = n log n + c2, we can then assume that the base of the
log is 2 and can conclude that the runtime of merge sort is O(n * log_2 (n))

*)

(*Solution to Problem 2*)
type color = R | B

type 'a rbtree =
    Empty
  | Node of color * 'a * 'a rbtree * 'a rbtree

(*Part 1*)

(*Function to ensure that the red parent has correct children*)
let checkCol =
    function
        |Empty -> true
        |Node (col, i, l, r) -> col = B (*check if child is black*)

(*to check whether the node is red or not*)
let rec checkRed t =
    match t with
        |Empty -> true
        |Node (B, i, l, r) -> checkRed l && checkRed r (*not red check both children*)
        |Node (R, i, l, r) ->
            if (checkCol l && checkCol r) then
                true (*make sure that children of red are black or empty*)
            else false

(*to calculate the number of black nodes in the subtrees*)
let rec bHeight t x =
    match t with 
        |Empty -> Some x
        |Node (B, i, l, r) -> 
            let Some lHeight = bHeight l 0 in (*get num of black nodes in left subtree*)
            let Some rHeight = bHeight r 0 in (*get num of black nodes in right subtree*)
                if (lHeight = rHeight) then
                    Some (2 * lHeight + 1) (*return the number*)
                else 
                    None (*not a rb tree*)
        |Node (R, i, l, r) -> 
            let Some lHeight = bHeight l 0 in (*get num of black nodes in left subtree*)
            let Some rHeight = bHeight r 0 in (*get num of black nodes in right subtree*)
                if (lHeight = rHeight) then
                    Some (2 * lHeight) (*return the number*)
                else 
                    None (*not a rb tree*)

let is_RBTree_aux t = 
    match t with 
        |Empty -> (0, true)
        |Node (B, i, l, r) ->
            let lHeight = bHeight l 0 in (*get num of black nodes in left subtree*)
            let rHeight = bHeight r 0 in (*get num of black nodes in right subtree*)
                (match lHeight with 
                    |Some x ->
                        (match rHeight with
                            |Some y -> if (x = y) then
                                            (x + 1, true) (*it is rb tree*)
                                       else (0, false) (*not rb tree*)
                            | None -> (0, false)) (*not rb tree*)
                    |None -> (0, false)) (*not rb tree*)

        |Node (R, i, l, r) -> 
            if (checkRed l && checkRed r) then (*Enusre that red node children are correct*)
                let lHeight = bHeight l 0 in (*get num of black nodes in left subtree*)
                let rHeight = bHeight r 0 in (*get num of black nodes in right subtree*)
                    (match lHeight with 
                        |Some x ->
                            (match rHeight with
                                |Some y -> if (x = y) then
                                                (x, true) (*it is rb tree*)
                                           else (0, false) (*not rb tree*)
                                | None -> (0, false)) (*not rb tree*)
                        |None -> (0, false)) (*not rb tree*)
            else (0, false) (*not rb tree*)

(*Part 2*)
(*get the tuple and return the bool value*)
let is_RBTree t = 
    let (x, y) = is_RBTree_aux t in y

(*Part 3*)
(*get the tuple and return the int value*)
let bh_RBTree t =
    if (is_RBTree t) then (*check if it is rb tree*)
        let (x, y) = is_RBTree_aux t in
            Some x (*return the count*)
    else 
        None (*not valid*)