
let writeLaTeX qmatrix amatrix =
  let file = "pdf/example.tex"
  and prefix = [
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
    let oc = open_out file
    in
      let writeStrings slist =
        let rec iter = function
          [] -> ()
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

let generate () =
  let (qm, am) = Spelling.placeAllStrings [ "dog"; "cat"; "tiger"; "man"; "zebra"; "parrot" ]
  in
    writeLaTeX qm am
