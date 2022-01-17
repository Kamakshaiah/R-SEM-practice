g <- make_empty_graph() + vertices(letters[1:10]) + vertices("foo", "bar", "bar2", "foobar2")
g <- g + edge("a", "b")
g <- g + edges("foo", "bar", "bar2", "foobar2")
g <- g + edges(c("bar", "foo", "foobar2", "bar2"), color="red", weight=1:2)

g <- make_empty_graph() + vertices(letters[1:10])
g <- g + path("a", "b", "c", "d")
g <- g + path("e", "f", "g", weight=1:2, color="red")
g <- g + path(c("f", "c", "j", "d"), width=1:3, color="green")

plot(g)

g <- make_empty_graph(n = 5) %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5)) %>%
  set_edge_attr("color", value = "red") %>%
  add_edges(c(5,1), color = "green")
E(g)[[]]
plot(g)

g <- make_empty_graph() %>%
  add_vertices(3, color = "red") %>%
  add_vertices(2, color = "green") %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5))
g
V(g)[[]]
plot(g)

# diagram
library(diagram)

sem_plot <- function(n, self=TRUE, arrows=TRUE){
  
  names <- letters[1:n]
  M <- matrix(n, n, byrow = TRUE, data = 0)
  if(self == TRUE){
    diag(M) <- "1"
  }
  if (arrows == TRUE){
    for (i in 1:dim(M)[1]){M[i, 1] <- "1"}
  }
  
  plotmat(M, pos = c(1, n-1), name=names, box.type = "square", box.size = 0.04)
  
}


# diagrammer 

library(DiagrammeR)

a_g <- create_graph() %>% 
  add_node() %>%
  add_edge()


# sem 

library(psych)

fit <- fa(cor(semdat))
ss <- structure.sem(fit)
semfit <- sem(ss, cor(semdat), 3)
