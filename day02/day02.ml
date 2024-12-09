let file = Sys.argv.(1)

let read_file_rec =
    let ic = open_in file in
    let rec read_lines acc =
        try
          let line = input_line ic in
              read_lines (line :: acc)
        with End_of_file ->
          close_in ic;
          List.rev acc
    in
        read_lines []
;;

let ascending list =
    let rec aux prev = function
        | [] -> true
        | x :: xs -> if x >= prev + 1 && x <= prev + 3 then aux x xs else false
    in
    match list with
    | [] -> true
    | x ::xs -> aux x xs
;;

let descending list =
    let rec aux prev = function
        | [] -> true
        | x :: xs -> if x <= prev - 1 && x >= prev - 3 then aux x xs else false
    in
    match list with
    | [] -> true
    | x ::xs -> aux x xs
;;

let is_valid list =
    ascending list || descending list

let part1 =
    read_file_rec
    |> List.map (function report ->
           report 
        |> String.split_on_char ' '
        |> List.map int_of_string)
    |> List.filter is_valid
    |> List.length
;;

Printf.printf "part1: %d\n" part1;
