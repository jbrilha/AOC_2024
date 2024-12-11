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

let solve_part1 =
    lines
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter (( <> ) ""))
    |> List.map (function
         | [a; b] -> (int_of_string a, int_of_string b)
         | _ -> failwith "bad line")
    |> List.split
    |> fun (a, b) ->
    List.map2
      (fun a b -> abs (a - b))
      (List.sort Int.compare a) (List.sort Int.compare b)
    |> List.fold_left ( + ) 0

let solve_part2 =
    lines
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter (( <> ) ""))
    |> List.map (function
         | [a; b] -> (int_of_string a, int_of_string b)
         | _ -> failwith "bad line")
    |> fun pairs ->
    List.fold_left ( + ) 0
      (List.map
         (fun x -> x * List.length (List.filter (( = ) x) (List.map snd pairs)))
         (List.map fst pairs))

let print_results part1 part2 =
    Printf.printf "part1: %d\n" part1;
    Printf.printf "part2: %d\n" part2

let () = print_results solve_part1 solve_part2
