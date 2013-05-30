module E = Example.Example(Serveur)

let () =
if Sys.argv.(1)= "g" then
  Serveur.files_machine (int_of_string Sys.argv.(2)) (Sys.argv.(3))     

else
begin 
  Serveur.build_available  (Sys.argv.(2));
  if Sys.argv.(1) = "c" then 
  Serveur.go (int_of_string Sys.argv.(3)) () 
  else

 (E.K.run E.main )
end
