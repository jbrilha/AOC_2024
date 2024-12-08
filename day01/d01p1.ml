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

let sum =
    read_file_rec
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
;;

Printf.printf "%d" sum;
print_newline ()
