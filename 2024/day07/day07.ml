type expression = {
    value: int;
    comps: int list;
  }

let valid_expression_p1 (expr: expression) : bool =
  let (first_comp, rest_comps) = match expr.comps with
    | x :: y -> (x, y)
    | [] -> (0, []) in
  let rec loop (value: int) (acc: int) (comps: int list) : bool =
    (* Printf.printf "%d %d\n" value fc; *)
    match comps with
    | x :: y -> if acc > value then false else (loop value (acc+x) y) || (loop value (acc*x) y)
    | [] -> (value = acc)
  in
  loop expr.value first_comp rest_comps

let concat (a: int) (b: int) : int =
  let con_str = (string_of_int a) ^ (string_of_int b) in
  try int_of_string con_str with Failure m -> -1

let valid_expression_p2 (expr: expression) : bool =
  let (first_comp, rest_comps) = match expr.comps with
    | x :: y -> (x, y)
    | [] -> (0, []) in
  let rec loop (value: int) (acc: int) (comps: int list) : bool =
    (* Printf.printf "%d %d\n" value fc; *)
    match comps with
    | x :: y -> if acc > value then false else (loop value (acc+x) y) || (loop value (acc*x) y) || (loop value (concat acc x) y)
    | [] -> (value = acc)
  in
  loop expr.value first_comp rest_comps

let parse_line (line: string) : expression =
  let parts = String.split_on_char ':' line in
  let (value_str, comps_str) = match parts with
    | value_str :: comps_str :: rest -> (value_str, comps_str)
    | value_str :: no_comps -> raise Exit
    | [] -> raise Exit in
  let value = try int_of_string value_str with Failure m -> 0 in
  let comp_strs = String.split_on_char ' ' comps_str
                  |> List.filter (fun s -> s <> "") in
  let try_conv s = try int_of_string s with Failure m -> 0 in
  let comps = List.map try_conv comp_strs in
  {
    value = value;
    comps = comps
  }

let parse_input (filename: string) : expression list =
  let ichan = open_in filename in
  let read_line () = try Some (input_line ichan) with End_of_file -> None in
  let rec loop acc = match read_line () with
    | Some s -> loop (parse_line s :: acc)
    | None -> close_in ichan; List.rev acc in
  loop []

let () =
  let inputs = parse_input "input.txt" in
  let valid_acc valid = fun acc inp -> acc + (if valid inp then inp.value else 0) in
  let valid_acc_p1 = valid_acc valid_expression_p1 in
  let valid_acc_p2 = valid_acc valid_expression_p2 in
  let s1 = List.fold_left valid_acc_p1 0 inputs in
  let s2 = List.fold_left valid_acc_p2 0 inputs in
  Printf.printf "part1 result = %d\n" s1;
  Printf.printf "part2 result = %d\n" s2;
