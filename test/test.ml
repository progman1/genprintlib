let _=
  let x=1 in
  [%prs "testing..." x ];
  [%prs "testing..." (x,x) ];
  [%prs "testing..." (x,x,x) ];
  [%pr testing one two three x ];
  [%pr something (x,x) ];
  begin try
      Genprint.print "cannot call directly" (1,2);
    with e->
      print_endline @@ Printexc.to_string e
  end;
  print_endline "done"
