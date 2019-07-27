## 红楼梦文本挖掘之数据预处理####
## 主要用于文本文档的读取和构建
## 文本预处理
## 孙玉林；2016年10月31


## 加载所需要的包
library(jiebaR)
library(tm)
library(readr)
library(stringr)
library(ggplot2)
library(wordcloud2)
library(wordcloud)
library(GGally)
library(gridExtra)
library(scatterplot3d)
library(plotly)
library(fastcluster)
library(lda)
library(LDAvis)
library(igraph)
library(ggraph)

## 读取所需要的文件####
# ## 读取红楼梦的词典
# filename <-"./数据/红楼梦诗词123.txt"
# dictionry <- read_csv(file = filename,col_names = FALSE)
# ## 对词典去重
# dictionry <- unique(dictionry$X1)
# write(dictionry,"./数据/红楼梦词典.txt",sep = "\n")
## 读取停用词
filename <- "./数据/我的红楼梦停用词.txt"
mystopwords <- readLines(filename)
## 读取红楼梦
filename <-"./数据/红楼梦UTF82.txt"
Red_dream <- readLines(filename,encoding='UTF-8')
## 将读入的文档分章节####
#去除空白行
Red_dream <- Red_dream[!is.na(Red_dream)]
# Red_dream <- as.vector(Red_dream)
# Red_dream[is.na(Red_dream)]
## 删除卷数据
juan <- grep(Red_dream,pattern = "^第+.+卷")
Red_dream <- Red_dream[(-juan)]
## 找出每一章节的头部行数和尾部行数
## 每一章节的名字
Red_dreamname <- data.frame(name = Red_dream[grep(Red_dream,pattern = "^第+.+回")],
                            chapter = 1:120)
## 处理章节名
names <- data.frame(str_split(Red_dreamname$name,pattern = " ",simplify =TRUE))
Red_dreamname$chapter2 <- names$X1
Red_dreamname$Name <- apply(names[,2:3],1,str_c,collapse = ",")
## 每章的开始行数
Red_dreamname$chapbegin<- grep(Red_dream,pattern = "^第+.+回")
## 每章的结束行数
Red_dreamname$chapend <- c((Red_dreamname$chapbegin-1)[-1],length(Red_dream))
## 每章的段落长度
Red_dreamname$chaplen <- Red_dreamname$chapend - Red_dreamname$chapbegin
## 每章的内容
for (ii in 1:nrow(Red_dreamname)) {
  ## 将内容使用句号连接
  chapstrs <- str_c(Red_dream[(Red_dreamname$chapbegin[ii]+1):Red_dreamname$chapend[ii]],collapse = "")
  ## 剔除不必要的空格
  Red_dreamname$content[ii] <- str_replace_all(chapstrs,pattern = "[[:blank:]]",replacement = "")
}
## 每章节的内容
content <- Red_dreamname$content
Red_dreamname$content <- NULL
## 计算每章有多少个字
Red_dreamname$numchars <- nchar(content)


##-----------------------------------------------------------------------
## 对红楼梦进行分词####
Red_fen <- jiebaR::worker(type = "mix",user = "./数据/红楼梦词典.txt")
Fen_red <- apply_list(as.list(content),Red_fen)
## 去除停用词,使用并行的方法
library(parallel)
cl <- makeCluster(4)
Fen_red <- parLapply(cl = cl,Fen_red, filter_segment,filter_words=mystopwords)
stopCluster(cl)
# Fen_red <- lapply(Fen_red, filter_segment,filter_words=mystopwords)
## 每章节最终有多少个词
Fen_red2 <- lapply(Fen_red, unique) #去重
Red_dreamname$wordlen <- unlist(lapply(Fen_red2,length))
## 添加分组变量，前80章为1组，后40章为2组
Red_dreamname$Group <- factor(rep(c(1,2),times = c(80,40)),
                              labels = c("前80章","后40章"))


## 词频统计##-----------------------------------------------------------
## 1:构建文档－词项频数矩阵
corpus <- Corpus(VectorSource(Fen_red))
Red_dtm <- DocumentTermMatrix(corpus,control = list(wordLengths=c(1,Inf)))
Red_dtm
## 一共有4万多个词

## 2:词频统计
word_freq <- sort(colSums(as.matrix(Red_dtm)),decreasing = TRUE)
word_freq <- data.frame(word = names(word_freq),freq=word_freq,row.names = NULL)
word_freq$word <- as.factor(word_freq$word)
head(word_freq)



## 绘制词频图
nn <- 250
sum(word_freq$freq>=nn)
word_freq[word_freq$freq >= nn,] %>%
  ggplot(aes(x = word,y = freq)) +
  theme_bw(base_size = 12,base_family = "STKaiti") +
  geom_bar(stat = "identity",fill= "red",colour = "lightblue",alpha = 0.6) +
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 75,hjust = 1,size = 8)) +
  labs(x = "词项",y = "频数",title = "《红楼梦》词频图")

## 词云
sum(word_freq$freq>=60)

data.frame(word_freq[word_freq$freq>60,]) %>%
  letterCloud("R",wordSize = 12)

## 静态词云
layout(matrix(c(1, 2), nrow=2), heights=c(0.4, 4))
par(mar=rep(0, 4),family = "STKaiti")
plot.new()
text(x=0.5, y=0.3, "红楼梦词云\nMin=60")
wordcloud(words = word_freq$word, freq = word_freq$freq,
          scale = c(4,0.8),min.freq = 60,random.order=FALSE,
          family = "STKaiti",colors = brewer.pal(8,"Dark2"))

## 动态词云
data.frame(word_freq[word_freq$freq>60,]) %>%
  wordcloud2(color = 'random-dark',backgroundColor = "whirt",
             shape = 'star' )



## 对每章的内容进行探索分析####
## 对相关章节进行分析
## 每章节的段落长度
p1 <- ggplot(Red_dreamname,aes(x = chapter,y = chaplen)) +
  theme_bw(base_family = "STKaiti",base_size = 10) +
  geom_point(colour = "red",size = 1) +
  geom_line() +
  geom_text(aes(x = 25,y = 0.9*max(Red_dreamname$chaplen)),
            label="前80章",family = "STKaiti",colour = "Red") +
  geom_text(aes(x = 100,y = 0.9*max(Red_dreamname$chaplen)),
            label="后40章",family = "STKaiti",colour = "Red") +
  geom_vline(xintercept = 80.5,colour = "blue") +
  labs(x = "章节",y = "段数",title = "《红楼梦》每章段数")
## 每章节的字数
p2 <- ggplot(Red_dreamname,aes(x = chapter,y = numchars)) +
  theme_bw(base_family = "STKaiti",base_size = 10) +
  geom_point(colour = "red",size = 1) +
  geom_line() +
  geom_text(aes(x = 25,y = 0.9*max(Red_dreamname$numchars)),
            label="前80章",family = "STKaiti",colour = "Red") +
  geom_text(aes(x = 100,y = 0.9*max(Red_dreamname$numchars)),
            label="后40章",family = "STKaiti",colour = "Red") +
  geom_vline(xintercept = 80.5,colour = "blue") +
  labs(x = "章节",y = "字数",title = "《红楼梦》每章字数")

p3 <- ggplot(Red_dreamname,aes(x = chapter,y = wordlen)) +
  theme_bw(base_family = "STKaiti",base_size = 10) +
  geom_point(colour = "red",size = 1) +
  geom_line() +
  geom_text(aes(x = 25,y = 0.9*max(Red_dreamname$wordlen)),
            label="前80章",family = "STKaiti",colour = "Red") +
  geom_text(aes(x = 100,y = 0.9*max(Red_dreamname$wordlen)),
            label="后40章",family = "STKaiti",colour = "Red") +
  geom_vline(xintercept = 80.5,colour = "blue") +
  labs(x = "章节",y = "词数",title = "《红楼梦》每章词数")
## 绘制每一章节的平行坐标图
p4 <- ggparcoord(Red_dreamname,columns = 7:9,scale = "center",
                 groupColumn = "Group",showPoints = TRUE,
                 title = "《红楼梦》") +
  theme_bw(base_family = "STKaiti",base_size = 10) +
  theme(legend.position =  "bottom",axis.title.x = element_blank()) +
  scale_x_discrete(labels = c("断落数","字数","词数")) +
  ylab("中心化数据大小")
  
gridExtra::grid.arrange(p1,p2,p3,p4,ncol = 2)

## 对三个变量绘制三散点图，
par(family = "STKaiti",mfcol = c(1,1))
color <- rep(c("red","blue"),times = c(80,40))
pchs <- rep(c(21,22),times = c(80,40))
scatterplot3d(x =Red_dreamname$chaplen,y = Red_dreamname$numchars,
              z=Red_dreamname$wordlen,color = color,pch = pchs,
              xlab="断落数", ylab="字数", zlab="词数",scale.y=1,
              angle=30,main = "《红楼梦》")
legend("topleft", inset=.05,      # location and inset
       bty="n", cex=.8,              # suppress legend box, shrink text 50%
       title="章节",
       legend = c("前80章","后40章"),
       pch = c(21,22),
       col = c("red","blue"))


## 可交互三维散点图
plot_ly(Red_dreamname, x = ~chaplen, y = ~numchars, z = ~wordlen) %>% 
  add_markers(color = ~Group,text = ~paste("Name: ", name)) %>%
  layout(title = "《红楼梦》")
  

##矩阵散点图
Red_dreamname_mat <- Red_dreamname[c("chaplen","numchars","wordlen","Group")]
names(Red_dreamname_mat) <- c("断落数","字数","词数","章节")
ggscatmat(Red_dreamname_mat,columns = c("断落数","字数","词数"),color = "章节") +
  theme_bw(base_family = "STKaiti") +
  ggtitle("《红楼梦》")
## 三个变量进行聚类分析



## 对每章节进行聚类分析####
## 1:构建文档－词项tf-IDF矩阵
corpus2 <- Corpus(VectorSource(Fen_red))
Red_dtm_tfidf <- DocumentTermMatrix(corpus2,control = list(wordLengths=c(1,Inf),
                                                    weighting = weightTfIdf))
Red_dtm_tfidf
## 一共有4万多个词
## 降低tfidf矩阵的稀疏度
Red_dtm_tfidfr <- removeSparseTerms(Red_dtm_tfidf,0.95)
Red_dtm_tfidfr
## 只留下了3000多个关键的字

## 使用系统聚类对每个章节进行聚类
Red_dtm_tfidfr_mat <- as.matrix(Red_dtm_tfidfr)
# ggparcoord(Red_dtm_tfidfr_mat,columns = 1:20,scale = "center",
#            showPoints = FALSE,
#            title = "《红楼梦》") +
#   theme_bw(base_family = "STKaiti",base_size = 10) +
#   theme(axis.title.x = element_blank()) +
#   scale_x_discrete() +
#   ylab("中心化数据大小")

## 文本间的距离度量为夹角余弦距离
Red_dtm_tfidfr_dist <- proxy::dist(Red_dtm_tfidfr_mat,method ="cosine")

## 系统聚类，聚为两类
k = 6
Red_clust <- hclust(d = Red_dtm_tfidfr_dist,method = "average")
Red_clust$labels <- Red_dreamname$chapter2
## 可视化绘图
par(family = "STKaiti",cex = 0.6)
plot(Red_clust,
     main = '红楼梦章节聚类\nmethod = average',
     xlab = '', ylab = '', sub = '')
groups <- cutree(Red_clust, k=k)   # "k=" defines the number of clusters you are using   
rect.hclust(Red_clust, k=k, border="red") # draw dendogra
## 每组有多少章
table(groups)
# dfgroup <- as.data.frame(groups)


k = 5
Red_clust <- hclust(d = Red_dtm_tfidfr_dist,method = "ward.D2")
Red_clust$labels <- Red_dreamname$chapter2
## 可视化绘图
par(family = "STKaiti",cex = 0.6)
plot(Red_clust,
     main = '红楼梦章节聚类\nmethod = word.D2',
     xlab = '', ylab = '', sub = '')
groups <- cutree(Red_clust, k=k)   # "k=" defines the number of clusters you are using   
rect.hclust(Red_clust, k=k, border="red") # draw dendogra
## 每组有多少章
table(groups)



## 另一种可视化
Red_clust <- hclust(d = Red_dtm_tfidfr_dist,method = "ward.D2")
Red_Den <- as.dendrogram(Red_clust)
## 添加节点的章节分组信息
Red_Den <- dendrapply(Red_Den, function(d) {
  if(is.leaf(d)) 
    attr(d, 'nodePar') <- list(Group=Red_dreamname[as.integer(attr(d, 'label')),10])
  d
})

# Plotting this looks very much like ggplot2 except for the new geoms
ggraph(graph = Red_Den, layout = 'dendrogram', repel = TRUE, circular = TRUE, 
       ratio = 0.5) + 
  geom_edge_elbow() + 
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter=leaf, 
                     angle = nAngle(x, y), label = label), 
                 size=3, hjust='outward') + 
  geom_node_point(aes(filter=leaf, color=Group)) + 
  coord_fixed() + 
  ggforce::theme_no_axes() +
  theme_bw(base_family = "STKaiti") 







##--------------------------------------------------------------------
# dfgroup <- as.data.frame(groups)
## 绘制各章节的关系网络,连接权重为距离系数
# ## 构建连接矩阵
# Red_dist_cut <- as.matrix(Red_dtm_tfidfr)
# ## Transform Data into an Adjacency Matrix
# # change it to a Boolean matrix
# Red_dist_cut[Red_dist_cut>=1] <- 1
# # transform into a term-term adjacency matrix
# Red_dist_cut <- Red_dist_cut %*% t(Red_dist_cut)
# # inspect terms numbered 5 to 10
# # Red_dist_cut[5:10,5:10]
## ---------------------------------------------------------
summary(Red_dtm_tfidfr_dist)
threshoud <- 0.8
Red_dist_cut <- as.matrix(Red_dtm_tfidfr_dist)
for (ii in 1:dim(Red_dist_cut)[1]) {
  for (kk in 1:dim(Red_dist_cut)[2]) {
    ## 距离大于的则没有连接
    aa <- Red_dist_cut[ii,kk]
    ## 数值越小权重越大
    aa <- ifelse(aa >=threshoud,0,aa)
    aa <- abs(aa - threshoud)
    aa <- ifelse(aa < threshoud,aa+threshoud,0)
    Red_dist_cut[ii,kk] <- aa
  }
}
# # plot(as.vector(Red_dist_cut))
# # names(Red_dist_cut) <- Red_dreamname$chapter2
row.names(Red_dist_cut) <- Red_dreamname$chapter
## ---------------------------------------------------------

# build a graph from the above matrix
g <- graph.adjacency(Red_dist_cut, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- row.names(Red_dist_cut)
V(g)$degree <- degree(g)

## 绘制每章节的网络关系图
set.seed(3952)
par(family ="STKaiti",cex = 1)
layout1 <- layout.kamada.kawai(g)
plot(g, layout=layout1)
## 美化图形
V(g)$label.cex <- 1.2 * V(g)$degree / max(V(g)$degree) +0.4
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .9, 0, egam)
E(g)$width <- egam *4
# plot the graph in layout1
plot(g, layout=layout1,main = "《红楼梦》章节的关系")

layout2 <- layout.sphere(g)
plot(g, layout=layout2,main = "《红楼梦》章节的关系")
## 文字越大，说明与该章相关的章节数越多
## 连接的线越粗，说明联系越大



