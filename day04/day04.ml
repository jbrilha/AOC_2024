let mat =
    let ic = open_in Sys.argv.(1) in
    let rec read_lines acc =
        try
          let line = input_line ic in
          let char_array = Array.init (String.length line) (String.get line) in
              read_lines (char_array :: acc)
        with End_of_file ->
          close_in ic;
          List.rev acc
    in
        Array.of_list (read_lines [])

let rows = Array.length mat
let cols = Array.length mat.(0)
let xmas = [|'X'; 'M'; 'A'; 'S'|]

let directions =
    [(-1, 0); (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1)]

let check_xmas row col =
    let rec check_neighbors r c i (dir_r, dir_c) =
        if i >= 4 then
          true
        else if r >= rows || r < 0 then
          false
        else if c >= cols || c < 0 then
          false
        else if mat.(r).(c) = xmas.(i) && i < 4 then
          let dr = r + dir_r in
          let dc = c + dir_c in
              check_neighbors dr dc (i + 1) (dir_r, dir_c)
        else
          false
    in
        List.fold_left
          (fun acc dir ->
            if check_neighbors row col 0 dir then
              acc + 1
            else
              acc)
          0 directions

let check_x_mas row col =
    let check_mas r1 c1 r2 c2 =
        (mat.(r1).(c1) = 'M' && mat.(r2).(c2) = 'S')
        || (mat.(r1).(c1) = 'S' && mat.(r2).(c2) = 'M')
    in
        mat.(row).(col) = 'A'
        && check_mas (row - 1) (col - 1) (row + 1) (col + 1)
        && check_mas (row + 1) (col - 1) (row - 1) (col + 1)

let solve_part1 =
    let rec count_xmas r c acc =
        if r >= rows then
          acc
        else if c >= cols then
          count_xmas (r + 1) 0 acc
        else
          count_xmas r (c + 1) (acc + check_xmas r c)
    in
        count_xmas 0 0 0

let solve_part2 =
    let rec count_x_mas r c acc =
        if r >= rows - 1 || r < 1 then
          acc
        else if c >= cols - 1 || c < 1 then
          count_x_mas (r + 1) 1 acc
        else
          let n_acc =
              if check_x_mas r c then
                acc + 1
              else
                acc
          in
              count_x_mas r (c + 1) n_acc
    in
        count_x_mas 1 1 0

let print_results part1 part2 =
    Printf.printf "part1: %d\n" part1;
    Printf.printf "part2: %d\n" part2

let () = print_results solve_part1 solve_part2
