(*Solution to Part 2*)
let ch = ref ' ';;

let getChar infile = (ch := input_char infile);;

let lookupChar () = !ch;;

let rec skipSpace infile =
  if (lookupChar () = ' ') then (getChar infile; skipSpace infile)
  else ();;

let isDigit ch = (ch >= '0' && ch <= '9');;

(*Solution to Problem 1
getWhole : in_channel -> int

Pre: takes input from the in channel
Description: using the string input, it processes the input and
  returns the final value as an integer
Post: returns the translated value from an int to a string*)
let getWhole infile = 
    let rec wholeConvert num =
        let nCh = lookupChar() in 
            if isDigit(nCh) then 
                (getChar(infile); wholeConvert (num * 10 + 
                            (int_of_char (nCh) - int_of_char '0')))
            else num 
    in wholeConvert 0;;

let test_getWhole () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   skipSpace stdin;
   if (isDigit (lookupChar ()))
   then Printf.printf "Your input: %d\n" (getWhole stdin)
   else Printf.printf "Bad input\n");;

(*
digits : int -> int

Pre: takes in an integer value
Description: the function processes the integer value and 
  returns the value of the total number of digits in the value,
  by using recursive integer division by 10, going until the 
  original value reaches 0
Post: returns the size of the integer
*)
let digits num = 
    let rec digits' nNum numDigits = 
        if (nNum <> 0) then
            digits' (nNum/10) (numDigits+1)
        else numDigits
    in digits' num 0;; 


(*Solution to problem 2
getFrac : in_channel -> float

Pre: takes in a string from in channel
Description: the function calls getWhole to process the input 
  as a integer and return the value, then gets the number of digits
  in the integer value by using the function digits, and then
  finally uses a mathematical formula num/(10^(size (num))), to 
  return a float value, the conversion from int to float is possible
  because the integers are wrapped in float types. 
Post: returns the final float value 
*)
let getFrac infile = 
    let num = getWhole infile in 
        let digNum = digits num in 
            (float num) /. (10.0 ** (float digNum));;
        
let test_getFrac () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   skipSpace stdin;
   if (isDigit (lookupChar ()))
   then Printf.printf "Your input: %f\n" (getFrac stdin)
   else Printf.printf "Bad input\n");;

(*Solution to Problem 3
getFloat : in_channel -> float option

Pre: takes input from in channel
Description: ignores the spaces, then checks if the input contains
  a number, and then processes the first part as an integer whole number, 
  then checks if there is a '.' and if so process the remaining input
  as a float by calling getFrac, then finally sums up the 2 values and
  returns the float value.
Post: returns the final float value. 

Side Note: the -9999999 value is a sentinel value to ensure that the
  correct output is returned, this value is used to check whether the 
  whole number has been changed or not, if not then only process the 
  fraction part and return it, otherwise change the whole number and 
  return the sum of the whole number and the fraction. 
*)
let getFloat infile = 
    let rec getFloat' num =
        (skipSpace(infile);
            if (isDigit(lookupChar())) then
                getFloat' (getWhole(infile))
            else if (lookupChar() = '.') then
                (getChar(infile); 
                    if (isDigit(lookupChar())) then
                        (*if there was no whole number, 
                            get and return frac*)
                        (if (num = -9999999) then
                            Some ((getFrac(infile)))
                        else 
                        (*whole num + fraction*)
                            Some ((float) num +. (getFrac(infile)))
                        )
                    (*no fraction but whole number was changed*)    
                    else if (num <> -9999999) then Some ((float) num) 
                    else None
                )
            else None)
    in getFloat' (-9999999);;

let test_getFloat () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   match (getFloat stdin) with
   | None -> Printf.printf "Bad input\n"
   | (Some r) -> Printf.printf "Your input: %f\n" r)