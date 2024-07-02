open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let get_radius_average ~x ~y image radius =
  let x_start = Int.max (x - radius) 0 in
  let y_start = Int.max (y - radius) 0 in
  let x_end = Int.min (x + radius) (Image.width image - 1) in
  let y_end = Int.min (y + radius) (Image.height image - 1) in
  Image.slice image ~x_start ~x_end ~y_start ~y_end |> Image.mean_pixel
;;

let transform image ~radius : _ =
  Image.mapi image ~f:(fun ~x ~y _pixel ->
    get_radius_average ~x ~y image radius)
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
  (* This test uses existing files on the filesystem. *)
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let transformed_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
    |> transform ~radius:3
  in
  let _image =
    Image.mapi ref_image ~f:(fun ~x ~y original_pixel ->
      let transformed_pixel = Image.get transformed_image ~x ~y in
      match Pixel.equal original_pixel transformed_pixel with
      | false ->
        print_s
          [%message (original_pixel : Pixel.t) (transformed_pixel : Pixel.t)];
        original_pixel
      | true -> original_pixel)
  in
  ();
  [%expect {| |}]
;;
