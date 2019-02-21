library(dplyr)
library(visNetwork)

nodes <- data.frame(id = 1:4, group = c( "Mom", "Me","Dad", "Brother"))
edges <- data.frame(from = c(1,2,3,4), to = c(2,3,4,1))

visNetwork(nodes, edges, width = "100%") %>%
  visGroups(groupname = "Mom", shape = "icon", icon = list(code = "f182", size = 75)) %>%
  visGroups(groupname = "Brother", shape = "icon", icon = list(code = "f007", color = "red")) %>%
  visGroups(groupname = "Dad", shape = "icon", icon = list(code = "f183", color = "red", size = 75)) %>%
  visGroups(groupname = "Me", shape = "icon", icon = list(code = "f007", color = "purple")) %>%
  visLegend() %>%
  visEdges(length = 100,smooth = FALSE) %>%
  visLayout(randomSeed = 11)  %>%
  addFontAwesome()
