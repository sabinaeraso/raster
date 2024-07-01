open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _ ->
    let x_start = if x - radius < 0 then 0 else x - radius in
    let x_end =
      if x + radius > Image.width image - 1
      then Image.width image - 1
      else x + radius
    in
    let y_start = if y - radius < 0 then 0 else y - radius in
    let y_end =
      if y + radius > Image.height image - 1
      then Image.height image - 1
      else y + radius
    in
    Image.mean_pixel (Image.slice image ~x_start ~x_end ~y_start ~y_end))
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

let%expect_test "blur" =
  let image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let radius = 3 in
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let blurred = transform image ~radius in
  let acc = [] in
  let are_equal =
    Image.foldi blurred ~init:acc ~f:(fun ~x ~y acc pixel ->
      let other = Image.get ~x ~y reference in
      if not (Pixel.equal pixel other) then List.append acc [ x, y ] else acc)
  in
  match are_equal with
  | [] -> ()
  | _ ->
    let head = List.take are_equal 10 in
    print_s [%message (head : (int * int) list)]
;;
