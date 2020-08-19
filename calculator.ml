type operator = | Value of float
                | Minus 
                | Plus 
                | Multiply
                | Divide
                | Power
                | Root
                | LEFT
                | RIGHT




let transform_operators res = match res with 
                |'-' -> Minus 
                |'+' -> Plus 
                |'*' -> Multiply
                |'/' -> Divide
                |'^' -> Power
                |'_' -> Root
                |'(' -> LEFT
                |')' -> RIGHT
                |_->Value (float_of_string (String.make 1 res))


(*These 3 can be merged into one I think*)
let rec joinValues lista =
    match lista with
    | Value (a) :: Value (b) :: tl -> (joinValues (Value ((a *. 10.) +. b) ::tl))
    | hd::tl -> (hd:: (joinValues tl))
    |_ -> lista

let rec fixParentheses lista =
    match lista with
    | RIGHT :: Value (a) :: tl -> (fixParentheses (RIGHT :: Multiply :: Value (a) ::tl))
    | hd::tl -> (hd::(fixParentheses tl))
    |_ -> lista

let rec fixPriorites lista =
    match lista with
    | Value (a) :: Multiply :: Value (b) :: tl -> 
            (fixPriorites (LEFT :: Value (a) :: Multiply :: Value (b) :: RIGHT :: tl))
    | Value (a) :: Divide :: Value (b) :: tl -> 
            (fixPriorites (LEFT :: Value (a) :: Divide :: Value (b) :: RIGHT :: tl))
    |hd::tl -> (hd :: (fixPriorites tl))
    |_ -> lista

let rec transform express=
    
    let rec aux str n lista =
        if n < 0 then fixPriorites (fixParentheses( joinValues lista))
        else aux str (n-1) ((transform_operators(String.get str (n))) :: lista)

    in aux express ((String.length express)-1) []


let rec calculate ts = 

    match ts with 
        |LEFT::Value (a) :: ((Minus|Plus|Multiply|Divide|Power|Root|_) as op) :: Value (b) :: RIGHT :: tl-> 
                (match op with
                |Minus -> calculate (Value (a -. b) :: tl)
                |Plus -> calculate (Value (a +. b) :: tl)
                |Multiply -> calculate (Value (a *. b) ::tl)
                |Divide -> calculate (Value (a /. b) :: tl)
                |Power -> calculate (Value (a ** b) :: tl)
                |Root -> calculate (Value (a ** b) :: tl)
                |_-> calculate ts)
        |Value (a) :: ((Minus|Plus|Multiply|Divide|Power|Root|_) as op) :: Value (b) :: tl ->
                (match op with
                |Minus -> calculate (Value (a -. b) :: tl)
                |Plus -> calculate (Value (a +. b) :: tl)
                |Multiply -> calculate (Value (a *. b) ::tl)
                |Divide -> calculate (Value (a /. b) :: tl)
                |Power -> calculate (Value (a ** b) :: tl)
                |Root -> calculate (Value (a ** b) :: tl)
                |_-> calculate ts)
        |hd::tl -> (hd:: (calculate tl))
        |_->ts

    
let () =
    let result = calculate (joinValues (transform (read_line () ))) in
    match result with
    | [Value  (a)] -> print_float a
    | _ -> print_string "ERROR"
