## 红楼梦文本挖掘之数据预处理####
## 主要用于文本文档的读取和构建
## 分析与挖掘R中的人物关系
## 红楼梦中关键人物的社交网络
## 孙玉林；2016年10月31


## 如果在每个段落中人物同时出现，则频数权重加1

## 加载所需要的包
library(igraph)
library(ggplot2)
library(gridExtra)
library(networkD3)

## 读取数据
## 一共有149人出现的频次多余10次
Red_net <- read.csv("./数据/社交网络权重.csv")
Red_net[,1:2] <- apply(Red_net[,1:2],2,as.character)
Name_freq <- read.csv("./数据/红楼梦人物出现频次.csv")
Name_freq <- Name_freq[Name_freq$word %in%(union(unique(Red_net$First),
                                                 unique(Red_net$Second))),]
Name_freq$word <- as.character(Name_freq$word)
# union(unique(Red_net$First),unique(Red_net$Second))

## 可视化人出现的频次
p1 <- ggplot(Name_freq,aes(x = reorder(word,freq),y = freq)) +
  theme_bw(base_size = 9,base_family = "STKaiti")+
  geom_bar(stat = "identity",position = "dodge",fill = "lightblue") +
  theme(axis.text.x = element_text(size = 5,hjust = 1,angle = 90,vjust = 0.5),
        axis.title.x = element_blank()) +
  labs(x = "",y = "频数",title = "《红楼梦》中关键人物出现次数")

p2 <- ggplot(Name_freq[Name_freq$freq>80,],aes(x = reorder(word,freq),y = freq)) +
  theme_bw(base_size = 9,base_family = "STKaiti")+
  geom_bar(stat = "identity",position = "dodge",fill = "lightblue") +
  theme(axis.text.x = element_text(size = 9,hjust = 1,angle = 90,vjust = 0.5)) +
  labs(x = "人名",y = "频数")

grid.arrange(p1,p2,nrow = 2)


## -----------------------------------------------------------------
## 按照权重1，即章节权重分析人物的社交网络####
# 1:准备社交网络数据
chap_net <- Red_net[Red_net$chapweight > 10,c(1,2,3)]
names(chap_net) <- c("from","to","weight")

chap_vert <- Name_freq[Name_freq$word %in% as.character(union(unique(chap_net$from),
                   unique(chap_net$to))),]

chap_net <- graph_from_data_frame(chap_net,directed = FALSE,
                                  vertices = chap_vert)
chap_net
chap_net$name <- "《红楼梦》章节人物关系"
V(chap_net)$media
## 节点数目
vcount(chap_net)
## 边的数目
ecount(chap_net)
## 简化网络图
chap_net <- simplify(chap_net,remove.multiple = TRUE,remove.loops = TRUE,
                     edge.attr.comb = "mean")

## 查看节点的度
degrees <- data.frame(name = names(degree(chap_net)),
                      counts = (degree(chap_net)))

ggplot(degrees,aes(x = reorder(name,counts),y = counts)) +
  theme_bw(base_size = 11,base_family = "STKaiti")+
  geom_bar(stat = "identity",position = "dodge",fill = "lightblue") +
  theme(axis.text.x = element_text(size = 5,hjust = 1,angle = 90,vjust = 0.5),
        axis.title.x = element_blank()) +
  labs(x = "人名",y = "节点的度",title = "《红楼梦》")

## 判断事否为联通图
is.connected(chap_net)

## 计算图的直径
diameter(chap_net,directed = FALSE)

## 
set.seed(1234)
par(cex = 0.8,family = "STKaiti")
## 设置图层
layout1 <- layout.lgl(chap_net)
layout2 <- layout.kamada.kawai(chap_net)
layout3 <- layout.reingold.tilford(chap_net)
layout4 <- layout.fruchterman.reingold(chap_net)

#V(chap_net)$size <- Name_freq$freq/10
## 设置节点的字体
V(chap_net)$label.family <- "STKaiti"
E(chap_net)$width <- log10(E(chap_net)$weight) *2
egam <- (E(chap_net)$width) / max(E(chap_net)$width)
E(chap_net)$color <- rgb(1,0.5,0.5,egam)
V(chap_net)$size <- log(V(chap_net)$freq) * 2.5
plot(chap_net,layout = layout1,main = "《红楼梦》根据章节部分人物关系")
plot(chap_net,layout = layout2,main = "《红楼梦》根据章节部分人物关系")
plot(chap_net,layout = layout3,main = "《红楼梦》根据章节部分人物关系")
plot(chap_net,layout = layout4,main = "《红楼梦》根据章节部分人物关系")


## -----------------------------------------------------------------
## 按照权重2，即段落权重分析人物的社交网络####
# 1:准备社交网络数据
chap_net <- Red_net[Red_net$duanweight > 10,c(1,2,4)]
names(chap_net) <- c("from","to","weight")

chap_vert <- Name_freq[Name_freq$word %in% as.character(union(unique(chap_net$from),
                                                              unique(chap_net$to))),]

chap_net <- graph_from_data_frame(chap_net,directed = FALSE,
                                  vertices = chap_vert)
chap_net
chap_net$name <- "《红楼梦》章节人物关系"
V(chap_net)$media
## 节点数目
vcount(chap_net)
## 边的数目
ecount(chap_net)
## 简化网络图
chap_net <- simplify(chap_net,remove.multiple = TRUE,remove.loops = TRUE,
                     edge.attr.comb = "mean")

## 查看节点的度
degrees <- data.frame(name = names(degree(chap_net)),
                      counts = (degree(chap_net)))

ggplot(degrees,aes(x = reorder(name,counts),y = counts)) +
  theme_bw(base_size = 11,base_family = "STKaiti")+
  geom_bar(stat = "identity",position = "dodge",fill = "lightblue") +
  theme(axis.text.x = element_text(size = 5,hjust = 1,angle = 90,vjust = 0.5),
        axis.title.x = element_blank()) +
  labs(x = "人名",y = "节点的度",title = "《红楼梦》")

## 判断事否为联通图
is.connected(chap_net)

## 计算图的直径
diameter(chap_net,directed = FALSE)

## 
set.seed(1234)
par(cex = 0.8,family = "STKaiti")
## 设置图层
layout1 <- layout.lgl(chap_net)
layout2 <- layout.kamada.kawai(chap_net)
layout3 <- layout.reingold.tilford(chap_net)
layout4 <- layout.fruchterman.reingold(chap_net)

#V(chap_net)$size <- Name_freq$freq/10
## 设置节点的字体
V(chap_net)$label.family <- "STKaiti"
E(chap_net)$width <- log10(E(chap_net)$weight) *2
egam <- (E(chap_net)$width) / max(E(chap_net)$width)
E(chap_net)$color <- rgb(1,0.5,0.5,egam)
V(chap_net)$size <- log(V(chap_net)$freq) * 2.5
plot(chap_net,layout = layout1,main = "《红楼梦》根据段落部分人物关系")
plot(chap_net,layout = layout2,main = "《红楼梦》根据段落部分人物关系")
plot(chap_net,layout = layout3,main = "《红楼梦》根据段落部分人物关系")
plot(chap_net,layout = layout4,main = "《红楼梦》根据段落部分人物关系")



## -----------------------------------------------------------------
## 按照权重2，即段落权重分析人物的社交网络####
## 分析链接次数较大的人物
# 1:准备社交网络数据
chap_net <- Red_net[Red_net$duanweight > 50,c(1,2,4)]
names(chap_net) <- c("from","to","weight")

chap_vert <- Name_freq[Name_freq$word %in% as.character(union(unique(chap_net$from),
                                                              unique(chap_net$to))),]

chap_net <- graph_from_data_frame(chap_net,directed = FALSE,
                                  vertices = chap_vert)
chap_net
chap_net$name <- "《红楼梦》章节人物关系"
V(chap_net)$media
## 节点数目
vcount(chap_net)
## 边的数目
ecount(chap_net)
## 简化网络图
chap_net <- simplify(chap_net,remove.multiple = TRUE,remove.loops = TRUE,
                     edge.attr.comb = "mean")

## 查看节点的度
degrees <- data.frame(name = names(degree(chap_net)),
                      counts = (degree(chap_net)))

ggplot(degrees,aes(x = reorder(name,counts),y = counts)) +
  theme_bw(base_size = 11,base_family = "STKaiti")+
  geom_bar(stat = "identity",position = "dodge",fill = "lightblue") +
  theme(axis.text.x = element_text(size = 5,hjust = 1,angle = 90,vjust = 0.5),
        axis.title.x = element_blank()) +
  labs(x = "人名",y = "节点的度",title = "《红楼梦》")

## 判断事否为联通图
is.connected(chap_net)

## 计算图的直径
diameter(chap_net,directed = FALSE)

## 
set.seed(1234)
par(cex = 0.8,family = "STKaiti")
## 设置图层
layout1 <- layout.lgl(chap_net)
layout2 <- layout.kamada.kawai(chap_net)
layout3 <- layout.reingold.tilford(chap_net)
layout4 <- layout.fruchterman.reingold(chap_net)

#V(chap_net)$size <- Name_freq$freq/10
## 设置节点的字体
V(chap_net)$label.family <- "STKaiti"
E(chap_net)$width <- log10(E(chap_net)$weight) *2
egam <- (E(chap_net)$width) / max(E(chap_net)$width)
E(chap_net)$color <- rgb(1,0.5,0.5,egam)
V(chap_net)$size <- log(V(chap_net)$freq) * 2.5
plot(chap_net,layout = layout1,main = "《红楼梦》根据段落部分人物关系")
plot(chap_net,layout = layout2,main = "《红楼梦》根据段落部分人物关系")
plot(chap_net,layout = layout3,main = "《红楼梦》根据段落部分人物关系")
plot(chap_net,layout = layout4,main = "《红楼梦》根据段落部分人物关系")



##-------------------------------------------------------------------
library(networkD3)
library(igraph)

# Basic Graph
chap_net <- Red_net[Red_net$duanweight > 40,c(1,2,4)]
g <- graph.data.frame(chap_net, directed=F) # raw graph


## Make a vertices df
vertices<-data.frame(
  name = V(g)$name,
  group = edge.betweenness.community(g)$membership,
  betweenness = (betweenness(g,directed=F,normalized=T)*115)+0.1 #so size isn't tiny
) 
#nb. can also adjust nodesize with `radiusCalculation`

# create indices (indexing needs to be JS format)
chap_net$source.index = match(chap_net$First, vertices$name)-1
chap_net$target.index = match(chap_net$Second, vertices$name)-1


# supply a edgelist + nodelist
d3 = forceNetwork(Links = chap_net, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'group', # color nodes by group calculated earlier
                  charge = -200, # node repulsion
                  linkDistance = 20,
                  zoom = T, 
                  opacity = 1,
                  fontSize=24)

show(d3)

##-------------------------------------------------------------------

# Basic Graph
chap_net <- Red_net[Red_net$duanweight > 60,c(1,2,4)]
g <- graph.data.frame(chap_net, directed=F) # raw graph


## Make a vertices df
vertices<-data.frame(
  name = V(g)$name,
  group = edge.betweenness.community(g)$membership,
  betweenness = (betweenness(g,directed=F,normalized=T)*115)+0.1 #so size isn't tiny
) 
#nb. can also adjust nodesize with `radiusCalculation`

# create indices (indexing needs to be JS format)
chap_net$source.index = match(chap_net$First, vertices$name)-1
chap_net$target.index = match(chap_net$Second, vertices$name)-1


# supply a edgelist + nodelist
d3 = forceNetwork(Links = chap_net, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'group', # color nodes by group calculated earlier
                  charge = -200, # node repulsion
                  linkDistance = 20,
                  zoom = T, 
                  opacity = 1,
                  fontSize=24)

show(d3)
