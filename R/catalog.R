catalogResolve =
function(id, type = "uri", debug = FALSE)
{
   types = c("uri", "public", "system")
   i = pmatch(tolower(type), types)
   if(is.na(i))
     stop("don't recognize type. Must be one of ", paste(types, collapse = ", "))

   .Call("R_xmlCatalogResolve", as.character(id), i, as.logical(debug))
}  
