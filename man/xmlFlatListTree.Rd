\name{xmlFlatListTree}
\alias{xmlFlatListTree}
\alias{xmlHashTree}
\title{Constructors for trees stored as flat list of nodes with
    information about parents and children}
\description{

  These (and related internal) functions allow us to represent trees as
  a simple, non-hierarchical collection of nodes along with
  corresponding tables that identify the parent and child relationships.
  This is different from representing a tree as a list of lists of lists
  ...  in which each node has a list of its own children. In a
  functional language like R, it is not possible then for the children
  to be able to identify their parents.
  
  We use an environment to represent these flat trees.  Since these are
  mutable without requiring the change to be reassigned, we can modify a
  part of the tree locally without having to reassign the top-level
  object.

  We can use either a list (with names) to store the nodes or a hash
  table/associative array that uses names. There is a non-trivial
  performance difference.
}
\usage{
xmlHashTree(nodes = list(), parents = character(), children = list(), 
             env = new.env(TRUE))
xmlFlatListTree(nodes = list(), parents = character(), children = list(), env = new.env(), n = 200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodes}{ a collection of existing nodes that are to be added to
    the tree. These are used to initialize the tree. If this is
    specified, you must also specify \code{children} and \code{parents}.
    }
  \item{parents}{ the parent relationships for the nodes given by \code{nodes}.}
  \item{children}{the children relationships for the nodes given by \code{nodes}.}
  \item{env}{an environment in which the information for the tree  will
    be stored. This is essentially the tree object as it allows us to
    modify parts of the tree without having to reassign the top-level
    object.    Unlike most R data types, environments are mutable.
   }
  \item{n}{for \code{xmlFlatListTree}, this is used as the default size
    to allocate for the list containing the nodes}
}
\details{

}
\value{
  An object of class XMLFlatTree
  which is specialized
  to \code{XMLFlatListTree} by the \code{xmlFlatListTree}
  function and
  \code{XMLHashTree} by the \code{xmlHasTree} function.
  Both are simply the environment.
}
\references{\url{http://www.w3.org/XML}}
\author{ Duncan Temple Lang }

\seealso{
  \code{\link{xmlTreeParse}}
  \code{\link{xmlTree}}
  \code{\link{xmlOutputBuffer}}
  \code{\link{xmlOutputDOM}}    
}
\examples{

}
\keyword{IO}
\concept{XML}