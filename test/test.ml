open! Core
open! Expect_test_helpers_core
open! Image_exercise_lib

(* let%expect_test "grayscale" = (* This test uses existing files on the
   filesystem. *) let ref_image = Image.load_ppm
   ~filename:"../images/reference-beach_portrait_gray.ppm" in let
   transformed_image = Image.load_ppm
   ~filename:"../images/beach_portrait_gray.ppm" |> transform in let
   diff_list = Image.foldi ref_image ~init:[] ~f:(fun ~x ~y diff_list pixel
   -> let tr_pixel = Image.get transformed_image ~x ~y in match Pixel.equal
   pixel tr_pixel with | false -> tr_pixel :: diff_list | true -> diff_list)
   in (* print_s [%message (List.length diff_list : int)]; *) List.iter
   diff_list ~f:(fun pixel -> print_s [%message (pixel : Pixel.t)]); (*
   print_s [%message (Pixel.equal reference_pixel transformed_pixel : bool)];
   *) (* [%expect {| ("List.length diff_list" 0) |}] *) [%expect {| |}] ;; *)
