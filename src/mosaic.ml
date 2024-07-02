open! Core

let calculate_mse (region1 : Image.t) (region2 : Image.t) =
  let width = Image.width region1 in
  let height = Image.height region1 in
  let summed =
    Image.foldi
      region1
      ~init:0
      ~f:(fun ~x ~y current_mse (red1, green1, blue1) ->
        let red2, green2, blue2 = Image.get region2 ~x ~y in
        let pixel_mse =
          Int.pow (red1 - red2) 2
          + Int.pow (green1 - green2) 2
          + Int.pow (blue1 - blue2) 2
        in
        current_mse + pixel_mse)
  in
  let final_mse = Float.of_int summed /. Float.of_int (width + height) in
  if Float.equal final_mse 0. then Float.max_value else final_mse
;;

(* print_s [%message (final_mse : int)]; *)

let divide_image image ~width ~height =
  let num_xs = Image.width image / width in
  let num_ys = Image.height image / height in
  let x_values = List.init num_xs ~f:(fun x_start -> x_start * width) in
  let y_values = List.init num_ys ~f:(fun y_start -> y_start * height) in
  List.fold x_values ~init:[] ~f:(fun slice_list x_start ->
    let x_end = x_start + width in
    List.fold y_values ~init:slice_list ~f:(fun slice_list y_start ->
      let y_end = y_start + height in
      (Image.slice image ~x_start ~y_start ~x_end ~y_end, (x_start, y_start))
      :: slice_list))
;;

(* Image.slice image ~x_start ~y_start ~x_end ~y_end *)

let switch_sections
  image
  ~section1:(section1_slice, (start_x1, start_y1))
  ~section2:(section2_slice, (start_x2, start_y2))
  =
  (* let end_x1 = start_x1 + width in let end_y1 = start_y1 + height in let
     end_x2 = start_x2 + width in let end_y2 = start_y2 + height in *)
  let _ =
    Image.mapi section1_slice ~f:(fun ~x ~y pixel1 ->
      let pixel2 = Image.get ~x ~y section2_slice in
      let image_x1 = start_x1 + x in
      let image_y1 = start_y1 + y in
      let image_x2 = start_x2 + x in
      let image_y2 = start_y2 + y in
      Image.set image ~x:image_x1 ~y:image_y1 pixel2;
      Image.set image ~x:image_x2 ~y:image_y2 pixel1;
      pixel1)
  in
  ()
;;

let transform image n =
  let width = 10 in
  let height = 10 in
  let slice_list = divide_image image ~width ~height in
  let _ =
    List.init n ~f:(fun _index ->
      let random_section = List.random_element_exn slice_list in
      let random_slice, _random_coords = random_section in
      let first_slice, first_coords = List.hd_exn slice_list in
      let first_mse = calculate_mse random_slice first_slice in
      (* print_s [%message (first_coords : int * int)]; *)
      let _lowest_mse, chosen_section =
        List.fold
          ~init:(first_mse, (first_slice, first_coords))
          slice_list
          ~f:
            (fun
              (best_mse, (best_section, best_coords))
              (curr_slice, curr_coords)
            ->
            let curr_mse = calculate_mse random_slice curr_slice in
            match Float.( < ) curr_mse best_mse with
            | true -> curr_mse, (curr_slice, curr_coords)
            | false -> best_mse, (best_section, best_coords))
      in
      (* let _, _chosen_coords = chosen_section in *)
      switch_sections image ~section1:random_section ~section2:chosen_section)
  in
  image
;;

let command =
  Command.basic
    ~summary:"Mosaic an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and n =
        flag "n" (required Command.Param.int) ~doc:"the number of iterations"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image n in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
