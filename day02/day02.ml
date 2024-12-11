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

let lines = read_file_rec

let ascending list =
    let rec aux prev = function
        | [] -> true
        | x :: xs ->
            if x >= prev + 1 && x <= prev + 3 then
              aux x xs
            else
              false
    in
        match list with
        | [] -> true
        | x :: xs -> aux x xs

let descending list =
    let rec aux prev = function
        | [] -> true
        | x :: xs ->
            if x <= prev - 1 && x >= prev - 3 then
              aux x xs
            else
              false
    in
        match list with
        | [] -> true
        | x :: xs -> aux x xs

let helper list =
    let remove_at i list = List.filteri (fun idx _ -> idx <> i) list in
    let rec try_removing i =
        if i >= List.length list then
          false
        else if remove_at i list |> descending || remove_at i list |> ascending
        then
          true
        else
          try_removing (i + 1)
    in
        try_removing 0

let solve_part2 =
    lines
    |> List.map (fun report ->
           report |> String.split_on_char ' ' |> List.map int_of_string)
    |> List.filter helper |> List.length

let solve_part1 =
    lines
    |> List.map (fun report ->
           report |> String.split_on_char ' ' |> List.map int_of_string)
    |> List.filter (fun list -> ascending list || descending list)
    |> List.length

let print_results part1 part2 =
    Printf.printf "part1: %d\n" part1;
    Printf.printf "part2: %d\n" part2

let () = print_results solve_part1 solve_part2
