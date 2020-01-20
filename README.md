# provGraphR

Creates an adjacency matrix from the provenance object returned by provParseR. The adjacency matrix can then be used to quickly traverse the provenance graph.

# Installation
Install from GitHub:
```{r}
# install.packages("devtools")
devtools::install_github("End-to-end-provenance/provGraphR")
```
Once installed, load the package:
```{r}
library("provGraphR")
```
# Usage
The create.graph function returns an object containing the adjacency graph and the parsed provenance from the prov.json file or string created by rdtLite or rdt.  The adjacency graph is a matrix with a row and column for each data and procedure node and a value of 1 if there is an edge from the column node to the row node and a value of 0 otherwise.  The get.prov function extracts the parsed provenance from the object returned by create.graph. The access functions below extract information about particular nodes. For example:

```{r}
adj.graph <- create.graph("c:/prov/prov.json")
prov <- get.prov("adj.graph")
lin.d10 <- get.lineage(adj.graph, "d10")
```
returns the adjacency graph "adj.graph" for the provenance file "c:/prov/prov.json", the associated parsed provenance "prov", and the lineage "lin.d10" of the data node "d10".

The following access functions return a string or a vector of strings. For more details, please see the help pages for provGraphR.

```{r}
DATA NODES

# Data nodes created by a procedure node
get.created.data

# Data nodes updated by a procedure node
get.updated.data

# Data nodes used by a procedure node
get.used.data

PROCEDURE NODES

# Procedure node that created a data node
get.creator

# Procedure nodes that used a data node
get.users

LINEAGE

# Forward or backward lineage of a node
get.lineage
```
