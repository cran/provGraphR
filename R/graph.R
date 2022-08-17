# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018, 2019, 2020, 2021, 2022.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.
#
# Authors: Orenna Bran, Joe Wonsil, Emery Boose, and Barbara Lerner 
#

#' @importClassesFrom provParseR ProvInfo
#' @importClassesFrom Matrix Matrix
#' @importFrom methods new
ProvGraphInfo <- methods::setClass("ProvGraphInfo",
    slots = list(
        adj.graph = "Matrix", 
        prov = "ProvInfo")
)

# This is called when a new ProvInfo object is created.  It initializes all of the slots.
methods::setMethod ("initialize",
    "ProvGraphInfo",
    function(.Object, prov, adj.graph){
      .Object@prov <- prov
      .Object@adj.graph <- adj.graph
      
      # Complete
      return (.Object)
    }
)


#' Create provenance graph
#' 
#' create.graph uses saved provenance to create an adjacency 
#' graph that captures the dependencies between
#' data and the R statements that use or modify the data.  
#' 
#' The graph contains a node for each R statement that is executed, for each 
#' variable set, and for each file read or written, and for each URL read.  There is 
#' an edge from each R statement node to the nodes representing variables set 
#' in that statement, or files written by the statement.  There is an edge from 
#' each variable node to the statement nodes that use the variable with that value.
#' There is also an edge from each input file or URL to the statement node that performs
#' the input operation.
#' 
#' The lineage of any data value can be traced through this graph by calling \code{\link{get.lineage}}.  
#'
#' @param prov.input This is either a file name, a string containing provenance
#'   collected by rdt or rdtLite, or parsed provenance.  The exact format of the JSON files is described in 
#'   \href{https://github.com/End-to-end-provenance/ExtendedProvJson/blob/master/JSON-format.md}{ExtendedProvJson.md}.
#' @param isFile A logical value indicating whether prov.input should be treated as a file name (isFile=TRUE) 
#'   or a string containing provenance (isFile=False). If prov.input is not a string, this parameter is ignored.
#' 
#' @return create.graph returns an object that contains the parsed provenance and a matrix representation of the graph.
#'   In the matrix, there is a row and a column for each
#'   data and procedure node in the graph.  The values in the matrix are either 1 or 0.
#'   A 1 indicates that there is an edge for the column node to the row node.  create.graph returns
#'   NULL if there is no provenance available.
#'
#' @export
#' @examples 
#' adj.graph <- create.graph(system.file("testdata", "basic.json", package = "provGraphR"))
#' @rdname creategraph
create.graph <- function(prov.input = NULL, isFile = TRUE){

  if (!is.null (prov.input) && is.character(prov.input)) {
    prov <- provParseR::prov.parse (prov.input, isFile)
  }
  
  proc.nodes <- provParseR::get.proc.nodes(prov)
  if (is.null (proc.nodes) || nrow(proc.nodes) == 0) {
    return (NULL)
  }
  
  # Collects all the ids of all the nodes that are put into the graph
  # Type is a vector of characters
  ids <- c(provParseR::get.proc.nodes(prov)$'id', provParseR::get.data.nodes(prov)$'id')
  
  # Collects the connections between nodes that exist in the prov
  # Originally there will be 3 columns including the ids, so
  # subset out the two columns that are required for the graph
  # Type is a matrix so that the graph can be subset by it
  proc.data.edges <- provParseR::get.proc.data(prov)[c("entity", "activity")]
  data.proc.edges <- provParseR::get.data.proc(prov)[c("activity", "entity")]
  edges <- as.matrix(rbind(proc.data.edges,
          stats::setNames(rev(data.proc.edges), names(data.proc.edges))))
  
  if (nrow(edges) == 0) return (NULL)
  
  # Create the graph, populating each element with zeros by the length of nodes
  adj.graph <- Matrix::Matrix(0, nrow = length(ids), ncol = length(ids), sparse =TRUE)
  
  # Make sure the resulting matrix is labelled for grabbing the right node
  rownames(adj.graph) <- colnames(adj.graph) <- ids
  
  # Sets all connections to 1 by subsetting by the edges matrix
  apply(edges, 1, function(edge){
        adj.graph[edge[1], edge[2]] <<- 1
      })
  
  
  return (methods::new (Class = "ProvGraphInfo", prov, adj.graph))
}

#' Calculate lineage of a node
#' 
#' get.lineage returns either the list of nodes that the provided node depends
#' on (backward lineage) or the list of nodes that depend on the provided node 
#' (forward lineage).
#' 
#' Most commonly, the node passed in is a data node representing either a variable,
#' a file, or a plot.  Forward lineage reports everything computed from that variable
#' or file.  Backward lineage reports everything that contributed to the variable's 
#' value, the contents of an output file or plot.
#' 
#' @param adj.graph An adjacency graph to get the lineage from, or a ProvGraphInfo object.  The
#'   object can be created by a call to create.graph.
#' 
#' @param node.id The string id for a node that the lineage is
#'   being requested for
#'
#' @param forward Logical that states whether the search is going forward
#'   through the graph from the provided node, or backwards.
#' 
#' @return get.lineage returns the forward or backward lineage of the specified node.  The lineage
#'   is represented as a vector of strings, with each string being the id of a node in the lineage.
#'   The first entry in the returned vector is the node.id passed in.  The remaining entries
#'   form a path either forward or backward through the adjacency graph.
#' 
#' @export
#' @examples 
#' adj.graph <- create.graph(system.file("testdata", "basic.json", package = "provGraphR"))
#' get.lineage (adj.graph, "d24")
#' 
#' @seealso \code{\link{create.graph}}
get.lineage <- function(adj.graph, node.id, forward = FALSE){
  if (inherits (adj.graph, "ProvGraphInfo")) {
    adj.graph <- adj.graph@adj.graph
  }
  
  if(!forward){
    ig <- igraph::graph_from_adjacency_matrix(adj.graph)
  } else {
    ig <- igraph::graph_from_adjacency_matrix(Matrix::t(adj.graph))
  }

  lineage <- as.character(stats::na.omit(names(igraph::dfs(ig, node.id, "out" , unreachable = FALSE)$order)))
  
  # At this point we have all the right nodes.  We want to return them
  # sorted by proc id so they appear in the order that they executed.
  # The data nodes need to be inserted in the right order.
  
  proc.nodes <- lineage [startsWith(lineage, "p")]
  data.nodes <- lineage [startsWith(lineage, "d")]
      
  # Sort the proc nodes by their id
  sorted.proc.nodes <- sort (proc.nodes, decreasing = !forward)
  
  # Find out which nodes created each of the data nodes
  data.creators <- sapply (data.nodes, 
      function (data.node) {
        get.creator (adj.graph, data.node)
      }
  )

  # Find out the position of the data creators in the sorted list
  data.creator.indices <- match (data.creators, sorted.proc.nodes)

  # Add the data nodes to the end of the sorted proc nodes
  lineage <- c(sorted.proc.nodes, data.nodes)
  
  # Move the data nodes to be adjacent to the proc nodes that produce them.
  if (forward) {
    # Data node should follow its producer
    # The first node's data creator is not part of the lineage, and we want
    # it to come first.  The match above will have put NA for the first data creator
    # index.
    data.creator.indices[1] <- 0
    
    # Determine the order that we want the sorted lineage to be in.
    id <- c (seq_along(sorted.proc.nodes), data.creator.indices + 0.5)
  }
  else {
    # Data node should precede its producer
    id <- c (seq_along(sorted.proc.nodes), data.creator.indices - 0.5)
  }
  
  # Now sort the lineage based on the order we just produced.
  sorted.lineage <- lineage[order(id)]
  return (sorted.lineage)
}

#' Get provenance used to create the adjacency graph
#' 
#' get.prov returns the provenance that corresponds with the given 
#' adjacency graph
#' 
#' @param adj.graph the adjacency graph
#' 
#' @export
#' @rdname creategraph
get.prov <- function (adj.graph) {
  return (adj.graph@prov)
}

#' Get creators and users of data
#' 
#' get.creator finds the node that creates the given data node
#' 
#' A data node can represent a variable, a file, a plot, or a warning or error.  The creator
#' of the data node will be a procedure node representing the statment
#' that assigned the variable, wrote to the file, created the plot, or 
#' resulted in the error or warning.
#' 
#' @param adj.graph the adjacency matrix
#' 
#' @param data.node.id the id of the data node.
#' 
#' @return the id of the procedure node that created the specified data node.  
#'   Returns NULL if there is no node with the given id, the id is not for 
#'   a data node, or the data node does not have a creator.  The last case
#'   can occur, for example, if the data node represents an input file.
#' 
#' @export
#' @examples 
#' adj.graph <- create.graph(system.file("testdata", "basic.json", package = "provGraphR"))
#' get.creator (adj.graph, "d1")
#' 
#' @seealso \code{\link{create.graph}}
#' @rdname proc
get.creator <- function (adj.graph, data.node.id) {
  if (inherits (adj.graph, "ProvGraphInfo")) adj.graph <- adj.graph@adj.graph

  # Make sure it is a data node
  if (!startsWith (data.node.id, "d")) {
    return (NULL)
  }
  
  # Check that there is a node with the name
  node.names <- rownames (adj.graph)
  if (!data.node.id %in% node.names) return (NULL)
  
  proc.node <- attr(which(adj.graph[data.node.id,] == 1, arr.ind=TRUE), "names")
  if (length(proc.node) > 1) warning ("get.creator found more than one creator")
  if (length(proc.node) == 0) return (NULL)
  return (proc.node)
}

#' get.users finds the nodes that use the given data node
#' 
#' A data node can represent a variable or a file.  The users
#' of the data node will be procedure nodes representing the statment
#' that used the variable in an expression or read from the file.
#' 
#' @return the id of the procedure node that created the specified data node.  
#'   Returns NULL if there is no node with the given id, the id is not for 
#'   a data node, or the data node does not have any users.  The last case
#'   can occur, for example, if the data node represents an output file.
#' 
#' @export
#' @examples 
#' get.users (adj.graph, "d1")
#' 
#' @seealso \code{\link{create.graph}}
#' @rdname proc
get.users <- function (adj.graph, data.node.id) {
  if (inherits (adj.graph, "ProvGraphInfo")) adj.graph <- adj.graph@adj.graph
  
  # Make sure it is a data node
  if (!startsWith (data.node.id, "d")) {
    return (NULL)
  }
  
  # Check that there is a node with the name
  node.names <- rownames (adj.graph)
  if (!data.node.id %in% node.names) return (NULL)
  
  proc.nodes <- attr(which(adj.graph[, data.node.id] == 1, arr.ind=TRUE), "names")
  if (length(proc.nodes) == 0) return (NULL)
  
  return (proc.nodes) 
}

#' Get data used and created by a statement
#' 
#' get.used.data returns the data nodes that this procedure node uses
#' 
#' A procedure node represents a top-level statement.  The data used by the
#' statement are the variables used in expressions or input files or URLs 
#' read from.
#' 
#' @param adj.graph the adjacency matrix
#' 
#' @param proc.node.id the id of the procedure node.
#' 
#' @return the ids of the data nodes that are used by the specified procedure node.  
#'   Returns NULL if there is no node with the given id, the id is not for 
#'   a procedure node, or the procedure node does not use any data nodes.  The last case
#'   can occur, for example, if the procedure node represents a statement where 
#'   a constant is assigned to a variable.
#' 
#' @export
#' @examples 
#' adj.graph <- create.graph(system.file("testdata", "basic.json", package = "provGraphR"))
#' get.used.data (adj.graph, "p11")
#' 
#' @seealso \code{\link{create.graph}}
#' @rdname data
get.used.data <- function (adj.graph, proc.node.id) {
  if (inherits (adj.graph, "ProvGraphInfo")) adj.graph <- adj.graph@adj.graph
  
  # Make sure it is a procedure node
  if (!startsWith (proc.node.id, "p")) {
    return (NULL)
  }
  
  # Check that there is a node with the name
  node.names <- rownames (adj.graph)
  if (!proc.node.id %in% node.names) return (NULL)
  
  data.nodes <- attr(which(adj.graph[proc.node.id, ] == 1, arr.ind=TRUE), "names")
  if (length(data.nodes) == 0) return (NULL)
  
  return (data.nodes) 
}

#' get.created.data returns the data nodes that this procedure node creates
#' 
#' A procedure node represents a top-level statement.  The data created by the
#' statement can be the variables set, output files written to, plots created,
#' error or warning messages created.
#' 
#' @return the ids of the data nodes that are created by the specified procedure node.  
#'   Returns NULL if there is no node with the given id, the id is not for 
#'   a procedure node, or the procedure node does not create any data nodes.  The last case
#'   can occur, for example, if the procedure node represents a statement where 
#'   the statement prints a constant string.
#' 
#' @export
#' @examples 
#' get.created.data (adj.graph, "p11")
#' 
#' @seealso \code{\link{create.graph}}
#' @rdname data
get.created.data <- function (adj.graph, proc.node.id) {
  if (inherits (adj.graph, "ProvGraphInfo")) adj.graph <- adj.graph@adj.graph
  
  # Make sure it is a procedure node
  if (!startsWith (proc.node.id, "p")) {
    return (NULL)
  }
  
  # Check that there is a node with the name
  node.names <- rownames (adj.graph)
  if (!proc.node.id %in% node.names) return (NULL)
  
  data.nodes <- attr(which(adj.graph[, proc.node.id] == 1, arr.ind=TRUE), "names")
  if (length(data.nodes) == 0) return (NULL)
  
  return (data.nodes) 
}

#' get.updated.data returns pairs of data nodes where the proc.node both uses 
#' and creates data nodes that have the same name, like with a <- a + 1
#' 
#' @return the ids of the data nodes that are updated by the specified procedure node.  
#'   Returns NULL if there is no node with the given id, the id is not for 
#'   a procedure node, or the procedure node does not update any data.
#' 
#' @export
#' @examples 
#' get.updated.data (adj.graph, "p5")
#' 
#' @seealso \code{\link{create.graph}}
#' @rdname data
get.updated.data <- function (adj.graph, proc.node.id) {
  created.data <- get.created.data (adj.graph, proc.node.id)
  if (is.null (created.data)) return (NULL)
  
  used.data <- get.used.data (adj.graph, proc.node.id)
  if (is.null (used.data)) return (NULL)
  
  # Find variables that are both used and created by this proc node
  all.data.nodes <- provParseR::get.data.nodes (adj.graph@prov)
  created.data.nodes <- all.data.nodes [all.data.nodes$id %in% created.data, c("id", "name")]
  used.data.nodes <- all.data.nodes [all.data.nodes$id %in% used.data, c("id", "name")]
  updated.vars <- intersect (created.data.nodes$name, used.data.nodes$name)
  if (length(updated.vars) == 0) return (NULL)
  
  # Create a data frame with 3 columns for each variable in updated.vars:
  #  var    original    updated
  updated.uses <- sapply (updated.vars, 
      function (var) return (used.data.nodes[used.data.nodes$name == var, "id" ]))
  updated.creates <- sapply (updated.vars, 
      function (var) return (created.data.nodes[created.data.nodes$name == var, "id" ]))
  updated <- data.frame (updated.vars, updated.uses, updated.creates, stringsAsFactors = FALSE)
  colnames(updated) <- c("var", "original", "updated")
  
  return (updated) 
}
