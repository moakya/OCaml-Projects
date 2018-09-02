(*Solution to Problem 1*)
let rec map f l =
    match l with
        | [] -> []
        | (h::t) -> (f h) :: map f t

let sqr n = n * n

let rec take l n =
    match (l,n) with
        | ((_,0) | ([],_)) -> []
        | (h::t,n) -> h :: (take t (n-1))

(*
1.1
Call by Name

take (map sqr [1;2;3]) 2
take (sqr(1)::map sqr [2;3]) 2
sqr(1)::take (map sqr [2;3]) 1
sqr(1)::take (sqr(2)::map sqr [3]) 1
sqr(1)::sqr(2)::take (map sqr [3]) 0
sqr(1)::sqr(2)::[]
1::4::[]
[1::4]
[1;4]
**************************************************
1.2
Call by value

take (map sqr [1;2;3]) 2
take (sqr(1)::map sqr [2;3]) 2
take (1::sqr(2)::map sqr [3]) 2
take (1::4::sqr(3)::map sqr []) 2
take (1::4::9::[]) 2
1::take (4::9::[]) 1
1::4::take (9::[]) 0
1::4::[]
[1::4]
[1;4]
*)

(*Solution to Problem 2*)
type 'a stream = Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f

let nextStream (Stream f) = f ()

let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))

let natStream = (fromNStream 1)

(*Solution to Problem 2.1
gets the element in the stream by utilizing nextStream, after
which applies the function f to it and recurrs through the rest of the
stream to ensure that all the elements are mapped*)
let mapStream f s =
    let rec mapStream' t =
        let (x, rst) = nextStream t in 
            (*create a new stream*)
            mkStream (fun () -> (f x, mapStream' rst))
    in mapStream' s

(*Solution to problem 2.2
Passes in functions that compute the square and cube of the integer
respectively and using mapStream, creates a stream that results in
perfect squares and perfect cubes
*)
let squareStream = mapStream (fun x -> x * x) natStream

let cubeStream = mapStream (fun x -> x * x * x) natStream

(*Solution to problem 2.3
utilizing the squareStream and cubeStream functions, chcecks
whether the elements produced by the two are the same, if so
returns a stream containing it, otherwise if the squareStream element
is less than that of the cuberStream element, keeps track of the cube
element and finds the next one in squareStream, otherwise recurrs the
cubeStream and keep track of the squareStream, until a match is found*)
let squarecubeStream = 
   let rec squarecubeStream' ss cs = 
        let (x, rst) = nextStream ss in
        let (y, rst2) = nextStream cs in
            if (x = y) then 
                mkStream (fun () -> (x, squarecubeStream' rst rst2))
            else if (x < y) then 
                squarecubeStream' rst cs
            else 
                squarecubeStream' ss rst2
    in squarecubeStream' squareStream cubeStream

(*Solution to Problem 3*)
type 'a stream' = Stream' of 'a stream_aux ref
and 'a stream_aux =
        | Evald of ('a * 'a stream')
        | UnEvald of (unit -> 'a * 'a stream')

(*Solution to Problem 3.1*)
(*create a stream of type 'stream and ref*)
let mkStream' f = Stream' (ref (UnEvald f))

(*to make sure that the nextStream' will be evaluated correctly*)
let nextStream' (Stream' s) = 
    match !s with
        |Evald x -> x   (*return an evaluated stream*)
        (*Evaluate the next stream*)
        |UnEvald x -> let v = x () in (s := Evald v; v) 

(*Solution to Problem 3.2*)
(*utilizing the mkStream' function creates a new function to be
utilized by natStream' to ensure that the new stream will be a 
stream of all natural numbers*)
let rec fromNStream' n = mkStream' (fun () -> (n, fromNStream' (n+1)))

let natStream' = fromNStream' 1