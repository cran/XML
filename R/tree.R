nodeIdGenerator =
  #
  # Not currently used. See asXMLTreeNode and the alternative default
  #  argument for XMLHashTree instances which would allow us to use
  # this function. But then we'd have to deal XMLFlatListTree differently.
  #
function(suggestion = "", env) {
       # the check to see if suggestion is a name in env is very expensive? Is it?
     if(suggestion == "" || exists(suggestion, env, inherits = FALSE)) 
        as.character(length(objects(env))) # .count + 1) 
     else
        suggestion
  }


asXMLTreeNode =
function(node, env,
         id = get(".nodeIdGenerator", env)(xmlName(node)), # nodeIdGenerator(xmlName(node), env),   
         className = "XMLTreeNode")
{
  node$id = id
  node$env = env
  class(node) = c(className, class(node))
  node
}

addParentNode =
function(node, kids = character())
{
  if(!inherits(node, 'XMLTreeNode')) {
     node = asXMLTreeNode(node, .this)
  }
  id = node$id
  .children[[ id ]] <<-  kids
  .parents[ kids ] <<- id
  .nodes[[ id ]] <<- node

  id
}  


addNode.XMLFlatListTree =
function(node, parent)
{
  e = parent$env
  if(!("id" %in% names(unclass(node))))
       node$id = get(".nodeIdGenerator", e)(xmlName(node))

  node$env = parent$env
  
  id = node$id
  nodes <- get(".nodes", e)
  nodes[[ id ]] <- node
  assign(".nodes", nodes, e)  
  
  p = get(".parents", e)
  p[id] = parent$id
  assign(".parents", p, e)

  kids = get(".children", e)
  kids[[ parent$id ]] <- c(kids[[ parent$id ]] , node$id)
  assign(".children", kids, e)  
  
  node
}  




names.XMLFlatTree =
function(x) {
   names(get(".nodes", x))
}

"$.XMLFlatListTree" =
function(x, name) {
  get(".nodes", envir = x)[[name]]
}

xmlRoot.XMLFlatTree =
function(x, ...)
{
   p = get(".parents", x)
#XXX   
}  

xmlParent.XMLTreeNode =
function(x)
{
  p = get(".parents", x$env)
  idx = match(x$id, names(p))
  if(is.na(idx))
      return(NULL)

  get(".nodes", x$env)[[ p[x$id] ]]
}  


xmlChildren.XMLTreeNode =
function(x, addNames = TRUE)
{
  e = x$env
  kids = get(".children", e)
  nodes = get(".nodes", e)

  if(x$id %in% names(kids))
    nodes[ kids[[ x$id ]] ]
  else
    list()
}  


