(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
    let taco      = Bigint (Pos, [1; 2;3 ;4;5;6;7;8;9])


    let flip_sign s = 
        if s = Pos
            then Neg
            else Pos

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

let rec print_bigint_69' value char_count =
        match value with
        | [] -> printf ""; 0
        | car1::cdr1 -> 
            let cnt  = print_bigint_69' cdr1 char_count in
            if cnt = char_count
            then (printf "%d\\\n" car1; 0 )
            else (printf "%d" car1; (cnt + 1) );;

let print_bigint_69 (Bigint (sign, value)) =
        match value with
        | [] -> printf ""
        | car1::cdr1 -> if sign = Pos 
            then (printf "";
                print_bigint_69' cdr1 68;
                printf "%d" car1;  
                printf "\n") 
            else (printf "-" ;
                print_bigint_69' cdr1 67;
                printf "%d" car1;  
                printf "\n")


    let print_the_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let rec get_last_value int_list = 
        if (List.length int_list) = 1
        then car int_list
        else (get_last_value (cdr int_list))

    let rec remove_leading_zeros int_list = 
        if (car int_list) = 0
        then if (List.length int_list) = 1
            then [0] 
            else remove_leading_zeros (cdr int_list)
        else int_list

    
    let rm_zeros int_list = 
        let rlist = reverse int_list in
        let result = remove_leading_zeros rlist in
        (reverse result)

    let rec cmp' list1 list2 =
        if (List.length list1) > (List.length list2)
        then 1 
        else if (List.length list1) < (List.length list2)
            then -1
            else let val1 = (car list1) in
                 let val2 = (car list2) in
                 if val1 > val2 
                 then 1
                 else if val1 < val2
                    then -1
                    else if ((List.length list1) = 1)
                        then 0
                        else (cmp' (cdr list1) (cdr list2) )  ;; 

    let cmp lista1 lista2 =
        let list1 = reverse lista1 in
        let list2 = reverse lista2 in
        cmp' list1 list2;;
 

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec decement_next list1 = match list1 with
        | [] -> []
        | list1 ->
        if (car list1) = 0
        then let new_val = 9 in
            new_val :: (decement_next (cdr list1))
        else
            let new_val = ((car list1) - 1) in
            new_val :: (cdr list1);;

    let rec sub' list1 list2 = 
        match (list1, list2) with
        | list1, []   -> list1 
        | [], list2   -> list2
        | car1::cdr1, car2::cdr2 ->  ( 
          let sub = car1 - car2
          in if sub < 0 
            then (sub + radix)  :: sub'  (decement_next cdr1) cdr2 
            else  sub :: sub' cdr1 cdr2
            ) 

    let rec mul_across' list1 val1 carry = match list1 with
        | []            -> [carry]
        | car1::cdr1    ->
          let result = car1 * val1 + carry
          in  result mod radix :: mul_across' cdr1 val1 (result / radix)

    let rec mul_add_results' list1 list2 = match (list1, list2) with
        | [], []      -> []
        | list1, []   -> []
        | [], list2   -> []
        | car1::cdr1, car2::cdr2 -> add' 
            (mul_across' list1 car2 0)
            (0 :: (mul_add_results' list1 cdr2))
            0

    let rec mult_by_2 int_list =
        (rm_zeros (add' int_list int_list 0));;


    let rec divrem' (dividend, powerof2, divisor') =
        match (dividend, divisor') with
        | [], []       -> [0], [0]
        | [], divisor' -> [0], [0]
        | dividend, [] -> [0], [0]
        | dividend, divisor' -> 
        if  ((cmp divisor' dividend) = 1)
        then [0], (rm_zeros dividend)
        else let quotient, remainder =
            divrem' (dividend, 
                    (mult_by_2 powerof2), 
                    (mult_by_2 divisor') ) in
            if ( (cmp remainder  divisor') = -1)
                 then (rm_zeros quotient),(rm_zeros remainder)
                 else  
                    let q = (add' quotient powerof2 0) in 
                    let r = (sub' remainder  divisor') in
                    (rm_zeros q), (rm_zeros r) 
 
    let divrem (dividend, divisor') = 
        let x, y  = divrem' (dividend, [1], divisor') in x, y ;;

    let rec power' value1 value2 = 
        if value2 = [1]
        then value1
        else mul_add_results' 
            value1 
            (power' value1 (rm_zeros (sub' value2 [1])) )

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0 )
        else if (cmp value1 value2) = 1
            then Bigint (neg1, sub' value1 value2)
            else Bigint ((flip_sign neg1), sub' value2 value1)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then if neg1 = Neg
            then add ( Bigint(neg1,value1)) (Bigint(Pos, value2)) 
            else if (cmp value1 value2) = 1
                then Bigint (Pos, (rm_zeros (sub' value1 value2)) )
                else Bigint ( Neg, (rm_zeros (sub' value2 value1)) )
        else if neg1 = Neg
            then Bigint (Neg, add' value1 value2 0) 
            else Bigint (Pos, add' value1 value2 0)


    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then if (List.length value1) > (List.length value2)
             then Bigint (Pos, mul_add_results' value1 value2)
             else Bigint (Pos, mul_add_results' value2 value1)
        else if (List.length value1) > (List.length value2)
             then Bigint (Neg, mul_add_results' value1 value2)
             else Bigint (Neg, mul_add_results' value2 value1) 

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, _ =  divrem(value1, value2)  in
        if neg1 = neg2
        then (Bigint (Pos, (rm_zeros quotient) ))
        else (Bigint (Neg, (rm_zeros quotient) ))
        

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, remainder = divrem (value1, value2) in
         (Bigint (Pos, remainder))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg
        then (Bigint (Pos, [0]))
        else let result = (power' value1 value2) 
            in (Bigint (neg1,(rm_zeros result) ))
            

end

