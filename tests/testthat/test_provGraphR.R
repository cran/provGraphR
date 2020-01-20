library(provGraphR)
library(provParseR)
library(testthat)

# no procedure nodes
test_that( "no procedure nodes", {
	test.data.file <- system.file("testdata", "empty.json", package = "provGraphR")
	expect_null(create.graph(test.data.file))
})

# no proc-data or data-proc edges
test_that( "no data edges", {
  test.data.file <- system.file("testdata", "no-data-edges.json", package = "provGraphR")
  expect_null(create.graph(test.data.file))
})

test_that ("get.creator", {
    test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
    graph <- create.graph(test.data.file)
    expect_equal (get.creator (graph, "d1"), "p2")
    expect_null (get.creator (graph, "d100"))  # No such node
    expect_null (get.creator (graph, "p1"))   # Not a data node
    expect_null (get.creator (graph, "d5"))   # Input file -- no creator
})

test_that ("get.users", {
      test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
      graph <- create.graph(test.data.file)
      expect_equal (get.users (graph, "d5"), "p6")
      expect_setequal (get.users (graph, "d1"), c("p4", "p5"))
      expect_null (get.users (graph, "d3"))   # No users
      expect_null (get.users (graph, "d10"))  # No such node
      expect_null (get.users (graph, "p1")) # Not a data node
    })

test_that ("get.used.data", {
      test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
      graph <- create.graph(test.data.file)
      expect_equal (get.used.data (graph, "p5"), "d1")
      expect_setequal (get.used.data (graph, "p4"), c("d1", "d2"))
      expect_null (get.used.data (graph, "p2"))   # No used data
      expect_null (get.used.data (graph, "p10"))  # No such node
      expect_null (get.used.data (graph, "d1")) # Not a proc node
    })

test_that ("get.created.data", {
      test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
      graph <- create.graph(test.data.file)
      expect_equal (get.created.data (graph, "p2"), "d1")
      expect_setequal (get.created.data (graph, "p8"), c("d8", "d9"))
      expect_null (get.created.data (graph, "p10"))   # No created data
      expect_null (get.created.data (graph, "p10"))  # No such node
      expect_null (get.created.data (graph, "d1")) # Not a proc node
    })

test_that ("get.updated.data", {
      test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
      graph <- create.graph(test.data.file)
      expect_null (get.updated.data (graph, "p10"))   # No created data
      expect_null (get.used.data (graph, "p2"))   # No used data
      expect_null (get.updated.data (graph, "p10"))  # No such node
      expect_null (get.updated.data (graph, "d1")) # Not a proc node

      updates <- get.updated.data (graph, "p5") # a <- a + 1
      expect_equal (nrow(updates), 1)
      expect_equal (updates$var[1], "a")
      expect_equal (updates$original[1], "d1")
      expect_equal (updates$updated[1], "d4")
      expect_null (get.updated.data (graph, "p6"))  # Uses and creates data, but they are different
      
      updates <- get.updated.data (graph, "p7") # a <- a + a
      expect_equal (nrow(updates), 1)
      expect_equal (updates$var[1], "a")
      expect_equal (updates$original[1], "d4")
      expect_equal (updates$updated[1], "d7")
      
      updates <- get.updated.data (graph, "p8") # a <- b <- a + b
      expect_equal (nrow(updates), 2)
      expect_equal (updates$var[1], "a")
      expect_equal (updates$original[1], "d7")
      expect_equal (updates$updated[1], "d8")
      expect_equal (updates$var[2], "b")
      expect_equal (updates$original[2], "d2")
      expect_equal (updates$updated[2], "d9")
      
      updates <- get.updated.data (graph, "p9") # a <- a <- a + b
      expect_equal (nrow(updates), 1)
      expect_equal (updates$var[1], "a")
      expect_equal (updates$original[1], "d8")
      expect_equal (updates$updated[1], "d10")
    })

test_that ("already parsed prov", {
      test.data.file <- system.file ("testdata", "simple.json", package="provGraphR")
      parsed.prov <- provParseR::prov.parse(test.data.file)
      graph <- create.graph(test.data.file)
      expect_equal (get.creator (graph, "d1"), "p2")
      expect_equal (get.prov (graph), parsed.prov)
    })

## Loading test data
test_that ("larger test case", {
  test.data.file <- system.file("testdata", "basic.json", package = "provGraphR")
  adj.graph <- create.graph(test.data.file)
  expect_setequal (get.lineage (adj.graph, "d24"), 
    c("d18", "p19", "d19", "p21", "d20", "p23", "d22", "p24", "d23", "p25", "d24"))
  expect_setequal (get.lineage (adj.graph, "d3", forward=TRUE), 
    c("d3", "p6", "d5", "p7", "d6", "p14", "d13"))
})

## Testing order
test_that ("order", {
      test.data.file <- system.file("testdata", "order.json", package = "provGraphR")
      adj.graph <- create.graph(test.data.file)
      lineage <- get.lineage (adj.graph, "d4")
      expect_equal (lineage, 
          c("d4", "p5", "d3", "p4", "d2", "p3", "d1", "p2"))
      
      test.data.file <- system.file("testdata", "ForwardOrder.json", package = "provGraphR")
      adj.graph <- create.graph(test.data.file)
      lineage <- get.lineage (adj.graph, "d1", forward=TRUE)
      expect_equal (lineage, 
          c("d1", "p3", "d2", "p4", "d3", "p5", "d4"))
})
