toString.XMLNode <-
function(x, ...)
{
  con <- textConnection(".tempXMLOutput", "w")
  sink(con)
  print(x)
  sink()
  close(con)
  paste(.tempXMLOutput, collapse="\n")
}  
