#
#  This is an experiment to see if a simple hash of the values 
#  is faster.
#
#  Basically, we keep the parents, children and nodes
#  each as hash tables not a list.
#
xmlHashTree =
  #
  # Currently ignore the nodes, parents and children.
  #
function(nodes = list(), parents = character(), children = list(),
         env = new.env(TRUE))
{
    # function to generate a new node identifier.  Can be given the
    # proposed name and will then make one up if that conflicts with another
    # identifier.

  .count = 0

  # ability to be able to refer to this tree itself.
  # Not used here since the functions don't make use of the tt environment implicitly.
  # assign(".this", env, env)
  
    # We will store the children and parents as regular entries in these hash tables.
  env$.children = .children = new.env(TRUE)
  env$.parents = .parents = new.env(TRUE)

   #XXX we can do without this and make it a regular function
   # but we need to deal with XMLFlatListTree slightly different.
  f = function(suggestion = "") {
       # the check to see if suggestion is a name in env is very expensive.
     if(suggestion == "" || exists(suggestion, env, inherits = FALSE)) 
        as.character(.count + 1)  # can use length(tt)
     else
        suggestion
  }
  assign(".nodeIdGenerator", f, env)


  addNode =
     # This adds each new node.  
   function(node, parentId)
   {
     node = asXMLTreeNode(node, .this, className = "XMLHashTreeNode")
     id = node$id

     assign(id, node, env)
     .count <<- .count + 1

     if(inherits(parentId, "XMLHashTreeNode"))
        parentId = parentId$id     

    if(length(parentId)) {
        assign(id, parentId, env = .parents)
        if(exists(parentId, .children, inherits = FALSE))
           tmp = c(get(parentId, .children), id)
        else
           tmp = id
        assign(parentId, tmp, .children)
     }    
      
    return(node)
   }
   env$.addNode <- addNode

  
  # Create a .nodes vector with the names
  # of the node. And then makes a
  #

  .tidy = function() {
      idx <- idx - 1
      length(nodeSet) <- idx
      length(nodeNames) <- idx 
      names(nodeSet) <- nodeNames
      .nodes <<- nodeSet
      idx
  }
  #  environment(env$.tidy) <- env
  
  .this = structure(env, class = c("XMLHashTree", "XMLFlatTree"))

  .this
}


"$.XMLHashTree" =
function(x, name)
  get(name, x, inherits = FALSE)


xmlParent.XMLHashTreeNode =
  # To get the parent of the node 'obj', we have to look in the .parents object
  # for the variable with obj's node identifier and then get the corresponding
  # value which is the identifier of the parent.
function(x)
{
  p = get(".parents", x$env)
  idx = exists(x$id, p, inherits = FALSE)
  if(!idx)
      return(NULL)

  get(get(x$id, p), x$env)
}  


xmlChildren.XMLHashTreeNode =
  #
  # For a given node 'obj', we have to use its id to find the entry
  # in the .children hash table and then the resulting entry is a character
  # vector giving the ids of the child nodes of obj. So we have to resolve those
  # children id's back in the hash table for the actual nodes.
function(x, addNames = TRUE)
{
  e = x$env
  kids = get(".children", e)

  if(exists(x$id, kids, inherits = FALSE)) {
    ids = get(x$id,  kids, inherits = FALSE)
    nodes = lapply(ids, get, e, inherits = FALSE)
    names(nodes) = sapply(nodes, xmlName)
    nodes
  } else
    list()
}  

xmlSize.XMLHashTreeNode =
function(obj)
{
  length(xmlChildren(obj))
}


xmlSize.XMLHashTree =
function(obj)
{
  # 3 is the number of entries with a . prefix that we put there
  # for our own implementation purposes
  # We could calculate this as
  # length(grep("^.", objects(obj, all = TRUE))
  length(obj) - 3  
}  

xmlRoot.XMLHashTree =
function(x, ...)
{  
  id = setdiff(objects(x), objects(x[[".parents"]]))
  get(id, x)
}


addNode =
function(node, parent, to)
{
  UseMethod("addNode", to)
}

addNode.XMLHashTree =
function(node, parent, to)
{
  to[[".addNode"]](node, parent)
}

xmlRoot.XMLHashTree =
  #
  # This can return multiple roots
  #
function(x, ...)
{
  parents = get(".parents", x, inherits = FALSE)
  tops = objects(x)[ is.na(match(objects(x), objects(parents)))]

  ans = mget(tops, x)

  if(length(ans) == 1)
    ans[[1]]
  else
    ans
}


print.XMLHashTree =
function(x, ...)
{
  print(xmlRoot(x), ...)
}  


xmlElementsByTagName.XMLHashTree =
  #
  # non-recursive version only at present
  #
function(el, name, recursive = FALSE)
{
  kids = xmlChildren(el)
  if(!recursive) 
    return(kids [ sapply(kids, xmlName) == name ])
}  
