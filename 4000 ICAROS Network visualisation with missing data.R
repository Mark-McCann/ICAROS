# ============================
# FILE:   03_COVID19_NETWORK.R
# AUTHOR: FEDERICA BIANCHI
# DATE:   APRIL 2022
# ============================

### CLEANING ENVIRONMENT AND CONSOLE ###
rm(list = ls(all = TRUE)) 
cat("\014")               

### LOADING LIBRARIES ###
library(readxl)
library(tidygraph)
library(ggraph)
library(graphlayouts)
library(qgraph)
library(dplyr)  
library(tidyverse)
library(scales)
library(gtools)
library(ggforce)
library(igraph)
 
### SETTING COLOR PALETTE ###
library(RColorBrewer)
# display.brewer.all(n = NULL, type = "all", select = NULL, exact.n = TRUE, colorblindFriendly = FALSE)

### SETTING WORKING DIRECTORY ###
working_dir = "C:/Users/mmc78h/Documents/GitHub/ICAROS"

setwd(working_dir)
dir()
##Read data 
DF <- read.csv("ICAROS dataset with missing.csv")

## EDGE LIST 
EL <- subset(DF, select =  c(EGO_ID, ALTER_ID_1, TIE_RATING_15))

EL[EL == 0] = NA
# EL[EL == "NA"] = NA
EL$TIE_RATING_01 = ifelse(EL$TIE_RATING_15 < 3, 0, 1)
EL = drop_na(EL)

EL$TIE_RATING_15 = as.numeric(EL$TIE_RATING_15)
EL$TIE_RATING_01 = as.numeric(EL$TIE_RATING_01)

EL$LINE_TYPE = NA
EL$LINE_TYPE[EL$TIE_RATING_01 == 0] = "Bad"
EL$LINE_TYPE[EL$TIE_RATING_01 == 1] = "Good"

EL$LINE_TYPE = factor(EL$LINE_TYPE, levels = c("Good", "Bad"))
# class(el$LINE_TYPE); levels(el$LINE_TYPE)

## NODE LIST WITH ATTRIBUTES
# ego_gender = read_excel(file_name, sheet = "EGO_ATTRIBUTES")
ego_gender <- read.csv("ICAROS EGO KEY TABLE.csv")[ ,c("EGO_ID", "GENDER")]


ego_gender[is.na(ego_gender)] = "M"



# alter_type = read_excel(file_name, sheet = "alter_type")
alter_type = read.csv("ALTER TYPE.csv")

colnames(alter_type)

d <- read.csv("ICAROS dataset with missing.csv")

###Edgelist with edge weight
el.rating <- d[, c("EGO_ID","ALTER_ID_1", "TIE_RATING_15")]
colnames(el.rating) <- c("from","to","rating")
##Vertex list with attributes.

egolist <- d[,c("EGO_ID", "EGO_GENDER")]

egolist <- unique(egolist)
dim(egolist)

length(unique(d[,"EGO_ID"])) 

alterlist <- d[,c("ALTER_ID_1", "ALTER_TYPE_4")]
alterlist <- unique(alterlist)
dim(alterlist)

length(unique(d[,"ALTER_ID_1"])) 


colnames(egolist) <- c("ID", "alter_type")
colnames(alterlist) <- c("ID", "alter_type")

vlist <- rbind(egolist, alterlist)

vlist$alter_type[vlist$alter_type == "M"] <- "Men"
vlist$alter_type[vlist$alter_type == "F"] <- "Women"

table(vlist$alter_type)

vlist$alter_type <- factor(vlist$alter_type ,
  levels = c(
    "Men",
    "Women",
    "Addiction",
    "Peer Support",
    "Mental health",
    "Family/Friends",
    "GP Health",
    "Social Work" ,
    "Supplier",
    "Other Services"
  ))
  
levels(vlist$alter_type) <- c("Men",
"Women",
"Addiction",
"Peer Support",
"Mental health",
"Family/Friends",
"General Health",
"Social Work" ,
"Supplier",
"Other")



graph_1 = graph_from_data_frame(d = el.rating, vertices = vlist) 



## ::::::::::::::::::::::::::::::::::::::::::::::::::::::
## NETWORK VISUALIZATION #1 -> NODE COLOR == NODE_COLOR_1
## ::::::::::::::::::::::::::::::::::::::::::::::::::::::

V(graph_1)$degree = degree(graph_1, mode = "in")

set.seed(4422)

# class(layout_1)

## COLOR PALETTE FOR NODE_COLOR_1
men = "#000000"; women = "#FFFFFF"


my_palette = "RdBu"; n_palette = length(unique(V(graph_1)$alter_type)) 

 display.brewer.pal(n_palette, my_palette)
 show_col(brewer_pal(palette = my_palette)(n_palette))
 brewer.pal(n_palette, my_palette)

addiction    = brewer.pal(n_palette, my_palette)[1]
peer_support = brewer.pal(n_palette, my_palette)[10]
social_work  = brewer.pal(n_palette, my_palette)[3]
supplier     = brewer.pal(n_palette, my_palette)[7]
famfriends   = brewer.pal(n_palette, my_palette)[8]
genhealth    = brewer.pal(n_palette, my_palette)[9]
menthealth   = brewer.pal(n_palette, my_palette)[5]
other_services = "gray87"

color_palette = c("Addiction" = addiction, "Family/Friends" = famfriends, "GP Health" = genhealth, "Men" = men,
                    "Mental health" = menthealth, "Other services" =  other_services, "Peer Support" = peer_support, 
                    "Social Work" = social_work, "Supplier" = supplier, "Women" = women)

rm(alter_type)


E(graph_1)$change <- c("Better/No change","Worse")[(E(graph_1)$rating < 3) + 1]
#E(graph_1)$change <- factor(E(graph_1)$change, levels = c("Better/No change","Worse"))

table(E(graph_1)$change, E(graph_1)$rating)

set.seed(427)
ICAROS_fig1 <-
  ggraph(graph_1, layout = "fr", weights = rating * 1.5) + 
  geom_edge_link0(aes(edge_width = rating, edge_linetype = change), edge_color = "black", edge_alpha = 0.7) + 
  geom_node_point(aes(fill = alter_type, size = degree), shape = 21) +
  scale_edge_width_continuous(range = c(0.2, 2), guide = "none") + 
  scale_fill_manual(values = color_palette, limits = c(   "Men",
                                                          "Women",
                                                          "Addiction",
                                                          "Peer Support",
                                                          "Mental health",
                                                          "Family/Friends",
                                                          "General Health",
                                                          "Social Work" ,
                                                          "Supplier",
                                                          "Other Services")  ) +
  theme_graph() + 
  scale_size_continuous(range = c(2, 6), guide = "none") + 
  labs(fill = " ", edge_linetype = " ") +
  theme(legend.position = "right", legend.direction = "vertical",
        legend.justification = "right", 
        legend.box = "vertical", legend.box.just = "right",
        legend.text = element_text(size = 16)) +
  guides(fill = guide_legend(override.aes = list(size = 3), order = 1),
         change = guide_legend(order = 2)) +

print(ICAROS_fig1)
setwd("C:/Users/mmc78h/Documents/GitHub/ICAROS/Outputs")
ggsave(filename = "ICAROS figure 1 two missing gender set to men.jpeg", plot = ICAROS_fig1, width = 10 ,height = 10)

