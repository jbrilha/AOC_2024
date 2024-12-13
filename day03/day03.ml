let lines =
    let ic = open_in Sys.argv.(1) in
    let rec read_lines acc =
        try
          let line = input_line ic in
              read_lines (line :: acc)
        with End_of_file ->
          close_in ic;
          List.rev acc
    in
        read_lines []

let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

let do_mul str cursor =
    let len = String.length str in
    let sum =
        if len < 3 || len > 7 then
          None
        else
          let parts = String.split_on_char ',' str in
              match parts with
              | [l; r] -> (
                  try
                    let left = int_of_string l in
                    let right = int_of_string r in
                        Some (left, right)
                  with Failure _ -> None)
              | _ -> None
    in
        match sum with
        | Some (left, right) -> (cursor + len, left * right)
        | None -> (cursor + 1, 0)

let add_muls line =
    let len = String.length line in
    let rec add_muls_rec pos sum =
        if pos >= len - 8 then
          sum
        else if String.sub line pos 4 = "mul(" then
          let cursor = pos + 4 in
              if cursor + 3 <= len && is_digit (String.get line cursor) then
                let next_pos, mul_sum =
                    do_mul
                      (String.sub line cursor
                         (String.index_from line cursor ')' - cursor))
                      cursor
                in
                    add_muls_rec next_pos (sum + mul_sum)
              else
                add_muls_rec (pos + 1) sum
        else
          add_muls_rec (pos + 1) sum
    in
        add_muls_rec 0 0

let add_valid_muls line in_can_do =
    let len = String.length line in
    let rec add_muls_rec pos sum can_do =
        if pos >= len - 8 then
          (sum, can_do)
        else if String.sub line pos 4 = "do()" then
          add_muls_rec (pos + 4) sum true
        else if String.sub line pos 7 = "don't()" then
          add_muls_rec (pos + 7) sum false
        else if can_do && String.sub line pos 4 = "mul(" then
          let cursor = pos + 4 in
              if cursor + 3 <= len && is_digit (String.get line cursor) then
                let next_pos, mul_sum =
                    do_mul
                      (String.sub line cursor
                         (String.index_from line cursor ')' - cursor))
                      cursor
                in
                    add_muls_rec next_pos (sum + mul_sum) can_do
              else
                add_muls_rec (pos + 1) sum can_do
        else
          add_muls_rec (pos + 1) sum can_do
    in
        add_muls_rec 0 0 in_can_do

let solve_part1 = lines |> List.map add_muls |> List.fold_left ( + ) 0

let solve_part2 =
    let rec process_lines lines curr_sum can_do =
        match lines with
        | [] -> curr_sum
        | line :: rest ->
            let sum, last_can_do = add_valid_muls line can_do in
                process_lines rest (curr_sum + sum) last_can_do
    in
        process_lines lines 0 true

let print_results part1 part2 =
    Printf.printf "part1: %d\n" part1;
    Printf.printf "part2: %d\n" part2

let () = print_results solve_part1 solve_part2
