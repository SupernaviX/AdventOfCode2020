type 'a dlink = {
  mutable data: 'a;
  mutable next: 'a dlink;
  mutable prev: 'a dlink;
  wrongwarp: ('a, 'a dlink) Hashtbl.t;
}

let dlink_of_list list = 
  let first = List.hd list in
  let wrongwarp = Hashtbl.create (List.length list) in
  let rec head = {
    data = first;
    prev = head;
    next = head;
    wrongwarp = wrongwarp;
  } in
  Hashtbl.add wrongwarp head.data head;
  let insert data after =
    let oldnext = after.next in
    let cell = {
      data = data;
      prev = after;
      next = oldnext;
      wrongwarp = wrongwarp;
    } in
    after.next <- cell;
    oldnext.prev <- cell;
    Hashtbl.add wrongwarp cell.data cell;
    cell
  in
  let rec build list after =
    match list with
      | [] -> after
      | hd :: tl -> build tl (insert hd after)
  in
  let _ = build (List.tl list) head in
  head

let list_of_dlink dlink =
  let rec lod node =
    if node == dlink then []
    else List.append [node.data] (lod node.next)
  in
  List.append [dlink.data] (lod dlink.next)

let dlink_length dlink =
  Hashtbl.length dlink.wrongwarp

let rec dlink_next dlink n =
  if n = 0 then dlink
  else dlink_next (dlink.next) (n - 1)

let rec dlink_contains dlink_start dlink_end value =
  if dlink_start == dlink_end.next then false else
  if dlink_start.data = value then true else
  dlink_contains dlink_start.next dlink_end value

let dlink_from dlink value =
  Hashtbl.find dlink.wrongwarp value

let parse input =
  String.to_seq(input)
    |> Seq.map(String.make 1)
    |> Seq.map(int_of_string)
    |> List.of_seq

let smaller_cup x max =
  if x == 1 then max
  else x - 1

let rec dest_cup current ignore_start ignore_end =
  let length = dlink_length current in
  let smaller_value = smaller_cup current.data length in
  let smaller = dlink_from current smaller_value in
  if dlink_contains ignore_start ignore_end smaller_value
    then dest_cup smaller ignore_start ignore_end
    else smaller

let crabstep current =
  let src_start = current.next in
  let src_end = dlink_next src_start 2 in
  let dest_start = dest_cup current src_start src_end in
  let dest_end = dest_start.next in
  dest_start.next <- src_start;
  dest_end.prev <- src_end;
  src_start.prev.next <- src_end.next;
  src_end.next.prev <- src_start.prev;
  src_start.prev <- dest_start;
  src_end.next <- dest_end;
  current.next

let rec crabshuffle iterations cups =
  if iterations == 0 then () else begin
    let next_cups = crabstep cups in
    crabshuffle (iterations - 1) next_cups
  end

let part1 input =
  let cups = dlink_of_list input in
  crabshuffle 100 cups;
  let from_1 = dlink_from cups 1 in
  (List.tl (list_of_dlink from_1))
    |> List.map(string_of_int)
    |> String.concat("")

let part2 input =
  let input_length = List.length input in
  let missing_length = 1000000 - input_length in
  let missing_cups = List.init missing_length (fun i -> i + input_length + 1) in
  let all_cups = input @ missing_cups in
  let cups = dlink_of_list all_cups in
  crabshuffle 10000000 cups;
  let from_1 = dlink_from cups 1 in
  string_of_int (from_1.next.data * from_1.next.next.data)

let main =
  let input = parse Sys.argv.(1) in
  let answer = part1 input in
  print_endline answer;
  let answer2 = part2 input in
  print_endline answer2;
