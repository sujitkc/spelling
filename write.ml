
let writeLaTeX qmatrix amatrix outfile =
  let prefix = [
    "\\documentclass{beamer}";
    "\\usetheme{default}";
    "\\usepackage{xcolor}";
    "\\begin{document}";
    "\\begin{frame}{}";
    "\\begin{center}"
  ]
  and suffix = [
    "\\hline";
    "\\end{tabular}";
    "\\end{center}";
    "\\end{frame}";

    "\\end{document}";
  ]
  in
    let oc = open_out outfile
    in
      let writeStrings slist =
        let rec iter = function
          []     -> ()
        | h :: t ->
          begin
            (Printf.fprintf oc "%s\n" h);
            (iter t)
          end
        in
        iter slist
      and writeTable () =
        let writeTableHeader cols =
          let rec iter i =
          match i with
              0 -> ()
            | _ ->
              begin
                (Printf.fprintf oc "c @{\\hspace{0.5cm}} ");
                iter (i - 1)
              end
          in
            begin
              (Printf.fprintf oc "\\begin{tabular}{| ");
              iter (cols - 1);
              (Printf.fprintf oc "c |}\n\\hline\n");
            end
        and writeTableContents () =
          for row = 0 to ((Array.length qmatrix) - 1) do
            for col = 0 to ((Array.length qmatrix) - 1) do
              begin
                if amatrix.(row).(col) = '*' then
                  (Printf.fprintf oc "\\color<2->{lightgray}%c " qmatrix.(row).(col))
                else
                  (Printf.fprintf oc "\\color<2->{red}%c " qmatrix.(row).(col));
                if col = ((Array.length qmatrix) - 1) then
                  Printf.fprintf oc " \\\\\n"
                else
                  Printf.fprintf oc " & "
              end
            done;
          done
        in
        begin
          writeTableHeader (Array.length qmatrix);
          writeTableContents ()
        end
      in
        begin
          writeStrings prefix;
          writeTable ();
          writeStrings suffix;
          close_out oc
        end

let inputLine ic =
  try Some (input_line ic)
  with End_of_file -> None

let readWords ic =
  let rec iter acc =
    match inputLine ic with
    | Some line -> iter (line::acc)
    | None -> (List.rev acc)
  in
  iter []

let readInput filename =
  let ic = open_in filename in
    let lines = readWords ic in
      begin
        close_in ic;
        lines
      end

let generate infile outfile =
  let (qm, am) = Spelling.placeAllStrings (readInput infile)
  in
    writeLaTeX qm am outfile

let _ =
    let output filename =
      for i = 1 to 10 do
        let outfile = "output/" ^ filename ^ "-" ^ (string_of_int i) ^ ".tex"
        in generate (filename ^ ".txt") outfile
      done
    in
    begin
      output "animals";
      output "vegetables";
      output "fruits"
    end

(* Test cases *)
let printStringList slist =
  List.iter (fun s -> (print_string (s ^ "\n"))) slist

let test_readInput () =
  let infile = "input.txt"
  in
    let slist = (readInput infile)
    in
    printStringList slist; ()
