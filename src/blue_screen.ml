open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)

let is_blue pixel max =
  let r, g, b = pixel in
  if b > max / 4 then b > r + g else false
;;

(* returns true if it is blue and false if not *)

let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    if is_blue pixel (Image.max_val background)
    then Image.get ~x ~y background
    else pixel)
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

let%expect_test "blue_screen" =
  let foreground = Image.load_ppm ~filename:"../images/oz_bluescreen.ppm" in
  let background = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let reference =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let blue = transform ~foreground ~background in
  let acc = [] in
  let are_equal =
    Image.foldi blue ~init:acc ~f:(fun ~x ~y acc pixel ->
      let other = Image.get ~x ~y reference in
      if not (Pixel.equal pixel other) then List.append acc [ x, y ] else acc)
  in
  match are_equal with
  | [] -> ()
  | _ ->
    let head = List.take are_equal 10 in
    print_s [%message (head : (int * int) list)]
;;
