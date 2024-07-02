open Core

(* This should look familiar by now! *)

let process pixel max n =
  let diff = max / (n - 1) in
  let poss = List.init n ~f:(fun i -> i * diff) in
  let red, green, blue = pixel in
  let dif = Int.max_value, Int.max_value in
  (*diff,value*)
  let r =
    List.fold poss ~init:dif ~f:(fun dif value ->
      let diff, new_value = dif in
      if Int.abs (value - red) < diff
      then Int.abs (value - red), value
      else diff, new_value)
  in
  let _, new_red = r in
  let b =
    List.fold poss ~init:dif ~f:(fun dif value ->
      let diff, new_value = dif in
      if Int.abs (value - blue) < diff
      then Int.abs (value - blue), value
      else diff, new_value)
  in
  let _, new_blue = b in
  let g =
    List.fold poss ~init:dif ~f:(fun dif value ->
      let diff, new_value = dif in
      if Int.abs (value - green) < diff
      then Int.abs (value - green), value
      else diff, new_value)
  in
  let _, new_green = g in
  new_red, new_green, new_blue
;;

let change_adjacent gray_image red_error green_error blue_error x y =
  let red_right_const = Int.to_float red_error *. (7. /. 16.) in
  let green_right_const = Int.to_float green_error *. (7. /. 16.) in
  let blue_right_const = Int.to_float blue_error *. (7. /. 16.) in
  let red_left_const = Int.to_float red_error *. (3. /. 16.) in
  let green_left_const = Int.to_float green_error *. (3. /. 16.) in
  let blue_left_const = Int.to_float blue_error *. (3. /. 16.) in
  let red_below_const = Int.to_float red_error *. (5. /. 16.) in
  let green_below_const = Int.to_float green_error *. (5. /. 16.) in
  let blue_below_const = Int.to_float blue_error *. (5. /. 16.) in
  let red_bottom_right_const = Int.to_float red_error *. (1. /. 16.) in
  let green_bottom_right_const = Int.to_float green_error *. (1. /. 16.) in
  let blue_bottom_right_const = Int.to_float blue_error *. (1. /. 16.) in
  if x + 1 < Image.width gray_image
  then (
    let new_red =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x:(x + 1) ~y)
            + Float.to_int red_right_const)
           0)
        (Image.max_val gray_image)
    in
    let new_green =
      Int.min
        (Int.max
           (Pixel.green (Image.get gray_image ~x:(x + 1) ~y)
            + Float.to_int green_right_const)
           0)
        (Image.max_val gray_image)
    in
    let new_blue =
      Int.min
        (Int.max
           (Pixel.blue (Image.get gray_image ~x:(x + 1) ~y)
            + Float.to_int blue_right_const)
           0)
        (Image.max_val gray_image)
    in
    let right = new_red, new_green, new_blue in
    Image.set gray_image ~x:(x + 1) ~y right)
  else ();
  (* right *)
  if x - 1 >= 0 && y + 1 <= Image.height gray_image - 1
  then (
    let new_red =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
            + Float.to_int red_left_const)
           0)
        (Image.max_val gray_image)
    in
    let new_green =
      Int.min
        (Int.max
           (Pixel.green (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
            + Float.to_int green_left_const)
           0)
        (Image.max_val gray_image)
    in
    let new_blue =
      Int.min
        (Int.max
           (Pixel.blue (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
            + Float.to_int blue_left_const)
           0)
        (Image.max_val gray_image)
    in
    let left = new_red, new_green, new_blue in
    Image.set gray_image ~x:(x - 1) ~y:(y + 1) left)
  else ();
  (* bottom left *)
  if y + 1 <= Image.height gray_image - 1
  then (
    let new_red =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x ~y:(y + 1))
            + Float.to_int red_below_const)
           0)
        (Image.max_val gray_image)
    in
    let new_blue =
      Int.min
        (Int.max
           (Pixel.blue (Image.get gray_image ~x ~y:(y + 1))
            + Float.to_int blue_below_const)
           0)
        (Image.max_val gray_image)
    in
    let new_green =
      Int.min
        (Int.max
           (Pixel.green (Image.get gray_image ~x ~y:(y + 1))
            + Float.to_int green_below_const)
           0)
        (Image.max_val gray_image)
    in
    let below = new_red, new_green, new_blue in
    Image.set gray_image ~x ~y:(y + 1) below)
  else ();
  (* directily below *)
  if y + 1 <= Image.height gray_image - 1 && x + 1 < Image.width gray_image
  then (
    let new_red =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x:(x + 1) ~y:(y + 1))
            + Float.to_int red_bottom_right_const)
           0)
        (Image.max_val gray_image)
    in
    let new_green =
      Int.min
        (Int.max
           (Pixel.green (Image.get gray_image ~x:(x + 1) ~y:(y + 1))
            + Float.to_int green_bottom_right_const)
           0)
        (Image.max_val gray_image)
    in
    let new_blue =
      Int.min
        (Int.max
           (Pixel.blue (Image.get gray_image ~x:(x + 1) ~y:(y + 1))
            + Float.to_int blue_bottom_right_const)
           0)
        (Image.max_val gray_image)
    in
    let bottom_right = new_red, new_green, new_blue in
    Image.set gray_image ~x:(x + 1) ~y:(y + 1) bottom_right)
  else ();
  gray_image
;;

(*type mutable error_map = [(int*int*int)] *)

let transform image n =
  Image.foldi image ~init:image ~f:(fun ~x ~y image pixel ->
    let new_pixel = process pixel (Image.max_val image) n in
    Image.set image ~x ~y new_pixel;
    let red_error = Pixel.red pixel - Pixel.red new_pixel in
    let green_error = Pixel.green pixel - Pixel.green new_pixel in
    let blue_error = Pixel.blue pixel - Pixel.blue new_pixel in
    change_adjacent image red_error green_error blue_error x y)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and number =
        flag
          "number"
          (required Command.Param.int)
          ~doc:"N the number to use when dither (higher = more colors)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let transformed = transform image number in
        Image.save_ppm
          transformed
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

let%expect_test "dither" =
  let image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let reference =
    Image.load_ppm
      ~filename:"../images/reference-beach_portrait_dither_color.ppm"
  in
  let dithered = transform image 2 in
  let acc = [] in
  let are_equal =
    Image.foldi dithered ~init:acc ~f:(fun ~x ~y acc pixel ->
      let other = Image.get ~x ~y reference in
      if not (Pixel.equal pixel other) then List.append acc [ x, y ] else acc)
  in
  match are_equal with
  | [] -> ()
  | _ ->
    let head = List.take are_equal 10 in
    print_s [%message (head : (int * int) list)]
;;
