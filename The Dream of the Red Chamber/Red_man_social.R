## 红楼梦文本挖掘之数据预处理####
## 主要用于文本文档的读取和构建
## 分析与挖掘R中的人物关系
## 孙玉林；2016年10月31


## 如果在每个段落中人物同时出现，则频数权重加1

## 加载所需要的包
library(jiebaR)
library(tm)
library(readr)
library(stringr)
library(plyr)


## 读取所需要的文件####
## 读取停用词
filename <- "./数据/我的红楼梦停用词.txt"
mystopwords <- readLines(filename)
## 读取红楼梦
filename <-"./数据/红楼梦UTF82.txt"
Red_dream <- readLines(filename,encoding='UTF-8')
## 读取人名字典
filename <-"./数据/红楼梦人物.txt"
Red_man <- readLines(filename,encoding='UTF-8')
filename <-"./数据/红楼梦诗词123.txt"
dictionary <- readLines(filename,encoding='UTF-8')
sum(is.element(Red_man,dictionary))
Red_man <- Red_man[is.element(Red_man,dictionary)]


## 将读入的文档分章节####
#去除空白行
Red_dream <- Red_dream[!is.na(Red_dream)]
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
## 每段落的内容
content <- Red_dreamname$content
Red_dreamname$content <- NULL
## 计算每章有多少个字
Red_dreamname$numchars <- nchar(content)
##-----------------------------------------------------------------------
## 根据出现在同一章的座位权重
## 对红楼梦进行分词####
Red_fen <- jiebaR::worker(type = "mix",user = "./数据/红楼梦词典.txt")
Fen_red <- apply_list(as.list(content),Red_fen)
## 去除停用词,使用并行的方法
library(parallel)
cl <- makeCluster(4)
Fen_red <- parLapply(cl = cl,Fen_red, filter_segment,filter_words=mystopwords)
stopCluster(cl)
## 词频统计##-----------------------------------------------------------
## 1:构建文档－词项频数矩阵
corpus <- Corpus(VectorSource(Fen_red))
Red_dtm <- DocumentTermMatrix(corpus,control = list(wordLengths=c(1,Inf)))
Red_dtm
## 一共有4万多个词

## 2:词频统计
word_freq <- sort(colSums(as.matrix(Red_dtm)),decreasing = TRUE)
word_freq <- data.frame(word = names(word_freq),freq=word_freq,row.names = NULL)
# word_freq$word <- as.factor(word_freq$word)
## 
word_freq <- word_freq[is.element(word_freq$word,Red_man),]
summary(word_freq)
hist(word_freq$freq,breaks= 200)
sum(word_freq$freq >10)
## 只分析出现次数大于10词的人物
Red_man <- as.character(word_freq$word[word_freq$freq >10])
Red_man
## 生成两两人物的所有组合
Red_mansol <- t(combn(Red_man,2,simplify = FALSE))
Red_mansol <- plyr::ldply(Red_mansol)
names(Red_mansol) <- c("First","Second")

##-------------------------------------------------
## 判断每个组合在文档中出现的次数
## 函数
timesFre <- function(strss,fencisol){
  strs <- as.character(strss)
  # strs
  aa <- lapply(fencisol,is.element,el = strs)
  aa <- lapply(aa,sum)
  aa <- ifelse(aa == 2,1,0)
  # weight <- sum(aa)
  return(sum(aa))
}
##-------------------------------------------------
timesFre(Red_mansol[1,],fencisol = Fen_red)

system.time({
  weights <- apply(Red_mansol[1:100,],1,timesFre,fencisol = Fen_red)
})


system.time({
  weights <- apply(Red_mansol,1,timesFre,fencisol = Fen_red)
})


hist(weights)

# ## 判断每个组合在文档中出现的次数
# weights <- vector(mode = "numeric", length = nrow(Red_mansol))
# for (ii in 1:nrow(Red_mansol)) {
#   strs <- as.character(Red_mansol[ii,1:2])
#   weight <- 0
#   for(kk in 1:length(Fen_red)){
#     weight <- ifelse(sum(is.element(strs,Fen_red[[kk]])) == 2,weight+1,weight)
#   }
#   weights[ii] <- weight
# }
# summary(weights)
# table(weights)

# weights <- vector(mode = "numeric", length = nrow(Red_mansol))
# # ii <- 5159
# for (ii in 1:nrow(Red_mansol)) {
#   strs <- as.character(Red_mansol[ii,1:2])
#   # strs
#   aa <- lapply(Fen_red,is.element,el = strs)
#   aa <- lapply(aa,sum)
#   aa <- ifelse(aa == 2,1,0)
#   weights[ii] <- sum(aa)
# }
# summary(weights)
# table(weights)

Red_mansol$chapweight <- weights


##-----------------------------------------------------------------------
## 根据出现在同一断落中的频数权重
## 对红楼梦进行分词####
Red_fen <- jiebaR::worker(type = "mix",user = "./数据/红楼梦词典.txt")
Fen_red <- apply_list(as.list(Red_dream),Red_fen)
## 去除停用词,使用并行的方法
library(parallel)
cl <- makeCluster(4)
Fen_red <- parLapply(cl = cl,Fen_red, filter_segment,filter_words=mystopwords)
stopCluster(cl)

## 计算权重

# weights <- vector(mode = "numeric", length = nrow(Red_mansol))
# # ii <- 5159
# for (ii in 1:nrow(Red_mansol)) {
#   strs <- as.character(Red_mansol[ii,1:2])
#   # strs
#   aa <- lapply(Fen_red,is.element,el = strs)
#   aa <- lapply(aa,sum)
#   aa <- ifelse(aa == 2,1,0)
#   weights[ii] <- sum(aa)
# }
# summary(weights)
# table(weights)

weights <- apply(Red_mansol,1,timesFre,fencisol = Fen_red)

hist(weights)

Red_mansol$duanweight <- weights

write_csv(Red_mansol,"./数据/社交网络权重.csv")

