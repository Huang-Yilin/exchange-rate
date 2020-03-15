library("foreign")
depression <- read.spss("孕产妇数据库.sav")
depression2 <- as.data.frame(depression)
depression2$f6 <- f6
depression2$e11 <- e11
depression2$g630 <- g630
e1_13 <- depression2[c("g6","g621","g622","g623","g624","g625","g626","g627","g628","g629")]
e1_14 <- depression2[c("g7","g721","g722","g723","g724","g725","g726","g727","g728","g729")]
g730 <- e1_14$g730
depression2$g730 <- g730
depression2$g11 <- rowSums(depression2[c("g1","g2","g3","g4","g51","g52","g53","g54","g55","g630","g730","g8","g9","g10")])
e11_lable = cut(e11, breaks=c(-1,5,10,31),
           labels=c("no depression", "mild depression", "severe depression"), right=TRUE)
depression2$e11_lable <- e11_lable
e12 = cut(e11, breaks=c(-1,9.5,31),
                labels=c("0","1"), right=TRUE)
e13= cut(depression3$e11, breaks=c(-1,5.1,31),
         labels=c("0","1"), right=TRUE)
depression3$e13<- e13
depression2$e12 <- as.numeric(e11)
f7= cut(depression2$f6, breaks=c(-1,3.1,6.1,10.1),
          labels=c("家庭功能严重障碍","家庭功能中度障碍","家庭功能良好"), right=TRUE)
depression2$f7 <- f7
quantile(depression4$g11)

g12 = cut(depression2$g11, breaks=c(11,36.1,45.1,66),
          levels=c("1","2","3"),
          labels=c("社会支持少","具有一般社会支持","满意社会支持度"), right=TRUE)
depression2$g12 <- g12
depression3[depression3$a13>20,"a13"] <- NA
impute(depression3$a13,mean)#将家庭常住人口填充为均值
depression3$b58 <- b58
depression3$b18 <- b18
a33 <- ifelse(depression3$a32x==2017,1,0)#生活时间低于1年
depression3$a33 <- a33

library(plspm)
e1_101 <- na.omit(e1_11)
x1 <- plspm::alpha(e1_101[,1:10]);x1#e1-10即爱丁堡产后抑郁量表的信度
x1_1 <- psych::KMO(cor(e1_101[,1:10]));x1_1#即爱丁堡产后抑郁量表的效度
cortest.bartlett(e1_101[,1:10])
e1_120 <- na.omit(e1_12)
x2 <- plspm::alpha(e1_120[,1:5]);x2#e1-10即APGAR家庭功能量表的信度
x1_2 <- psych::KMO(cor(e1_120[,1:5]));x1_2#即APGAR家庭功能量表的效度
cortest.bartlett(e1_120[,1:5])
e1_101 <- na.omit(e1_11)
x3 <- plspm::alpha(e1_16);x3#SSRS量表的信度
x1_3 <- psych::KMO(cor(e1_16));x1_3#即SSRS量表的效度
cortest.bartlett(e1_16)
e1_16 <- depression2[c("g1","g2","g3","g4","g51","g52","g53","g54","g55","g630","g730","g8","g9","g10")]
depression2$g11_lable = cut(g11, breaks=c(11,40,61),
                            labels=c("社会支持较差", "社会支持较好"), right=TRUE)
#结果
x <- depression2$e11
curve(dnorm(x,mean(x),sd(x)))
depression3 <- depression2[-c(9,23,153,286,341,457,149,247),]#删除了产后6周(9,23,153,286,341,457)以上,抑郁量表有空值(149,247)
depression4 <- depression3[-c(20,21,22,32,50,60,64,69,79,83,90,91,95,98,107,112,117,132,137,147,153,213,223,225,241,282,
                              327,344,345,357,362,363,378,387,392,401,416,443),]#删除了不是农转非，

table(depression3$e12)

write.csv(depression4,file="/Users/xiaoyiyi/Desktop/R路径文件夹/data/孕产妇新.csv",na=" ",fileEncoding ="GB2312" )
t.test(a1 ~ e12, data = depression4)
library("tableone")
vars1 <- c('id1','a1','a4','a5','a6','a7','a8','a10','a11','a12','a13','b18','b58','b2','b3','b4','b9','a33',
           'Objective_support','a7_lable','Subjective_support','Support_utilization','f1','f2','f3','f4','f5',
           'c1','e12','g11','e11','f6','f7','g12','g11_lable')
#id1,a1,a4,a5,a6,a7,a8,a10,a11,a12,a13,b2,b3,b4,b9,b18,b58,a33,Objective_support,a7_lable,Subjective_support,Support_utilization,f1,f2,f3,f4,f5,c1,e12
vars2 <- c('id1','a33','a4','a5','a6','a7','a8','a10','a11','a12','b58','b18','b2','b3','b4','b9','a7_lable',
           'c1','e12','f7','g12','g11_lable')
temp <- CreateTableOne(vars = vars1,data = depression4,factorVars = vars2,strata = c('e12'))
temp <- print(temp)
write.csv(temp,file="/Users/xiaoyiyi/Desktop/R路径文件夹/data/temp1.csv",fileEncoding ="GB2312" )



b <- step(lm(e13~id1+a4+a5+a6+a7+a8+a10+a11+a12+b2+b3+b18+b58+b4+b9+
           c1+g11+f6,data=depression4))
a <- step(glm(e13~id1+a4+a5+a6+a7+a8+a13+b2+b3+b18+b58+b4+b9+a33+
                c1+f7+g11+f6+g12,family = binomial(link = "logit"),data=depression4,na.action = na.omit))
confint(a$coefficients,level = 0.95)
a <- summary(b)
??glm
table(depression4$a33)
library(nnet)
b <- step(multinom(e12~id1+a4+a5+a6+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+
                     c1+f7+g11+f6+g12+g11_lable,data=depression4,na.action = na.omit))
summary(b)
b <- step(glm((e13-1)~id1+a4+a5+a6+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+
                c1+f1+f2+f3+f4+f5+g11+g12+g11_lable,family = "binomial",data=depression4,na.action = na.omit))
anova(b)
table(depression4$e13)
summary(aov(depression4$e12~depression4$g11))

as.factor
table(depression4$a32x==2017,depression4$e12)
fore <- read.table(pipe("pbpaste"),header=T,sep="\t")
fore1 <- rbind(names(fore),fore)
fore$Pr...t..[1] <- "Pvalue"
fore$OR.95.CI. <- as.character(fore$OR.95.CI.)
fore1$OR <- as.numeric(fore1$OR )
library("forestplot")
fore1 <- rbind(c(rep(NA,10)),fore1)
forestplot(labeltext = fore[,c(1,5:7)],
             mean=fore1$OR ,lower = fore1$low95.CI,upper = fore1$high95.Cl,
           is.summary = c(T,F,F,F,F,F,F,F,F,F,F,F,F),
           zero=1,boxsize = 0.3,lineheight = unit(14,'mm'),
           colgap = unit(2,'mm'),col = fpColors(box='royalblue',lines='black',zero= '#7AC5CD'),
           xlab = 'The estimates',
           graph.pos = 3)
library("officer")
x <- paste(mean(depression4$e11),"x%+-%y",sd(depression4$e11))
summary(aov(depression4$e11~depression4$a33))
summary(aov(depression4$e12~depression4$a4))
summary(step(glm(e12~id1+a1+a4+a5+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+Objective_support+a7_lable+
                   Subjective_support+Support_utilization+f1+f2+f3+f4+f5+g11+g12+f6+f7+
                   c1,family = "binomial" ,data=depression4,na.action = na.omit)))
coefficients(aov(depression4$e12~depression4$a33))
summary(step(lm(e11~id1+a4+a5+a6+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+
                c1+f6+g11_lable,data=depression4,na.action = na.omit)))
table(depression4$a1,depression4$e12)
train <- depression4[depression4$id1==1,]
test <- depression4[depression4$id1==2,]
summary(step(glm(e12~id1+a1+a4+a5+a6+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+
                   c1+f6+f7+g11+g11_lable,family = "binomial" ,data=test,na.action = na.omit)))
temp1
table(depression4$n2)
library("readxl")
cor.test(depression4$f6,depression4$e12)
depression4$Objective_support <- rowSums(depression4[,c("g630","g730","g2")])
depression4$Subjective_support <- rowSums(depression4[,c("g1","g3","g4","g51","g52","g53","g54","g55")])
depression4$Support_utilization <- rowSums(depression4[,c("g8","g9","g10")])

summary(depression3$g11)
table(depression4$e11)
a7_lable = cut(depression4$a7, breaks=c(0,2.1,5.1,8.1),labels=c("1", "2","3"), right=FALSE)
depression4$a7_lable <- as.numeric(depression4$a7_lable)
selectyunfu=subset(depression4,id1=="1")
selectchanfu=subset(depression4,id1=="2")
summary(step(glm(e12~id1+a1+a4+a5+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+Objective_support+a7_lable+
                   Subjective_support+Support_utilization+f1+f2+f3+f4+f5+
                   c1,family = "binomial" ,data=selectyunfu,na.action = na.omit)))
summary(step(glm(e12~id1+a1+a4+a5+a6+a7+a8+a10+a11+a12+a13+b2+b3+b4+b9+b18+b58+a33+Objective_support+a7_lable+
                   Subjective_support+Support_utilization+f1+f2+f3+f4+f5+
                   c1,family = "binomial" ,data=depression4,na.action = na.omit)))
table(depression4$b2)
temp <- subset(depression4,select = c(g1,g2,g3,g4,g51,g52,g53,g54,g55,g630,g730,g8,g9,g10))
x3 <- plspm::alpha(temp)
psych::KMO(cor(temp))
temp <- subset(depression4,select = c(f1,f2,f3,f4,f5))
x2 <- plspm::alpha(temp)
psych::KMO(cor(temp))
temp <- subset(depression4,select = c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10))
x1 <- plspm::alpha(temp)
psych::KMO(cor(temp))
table(depression4$a4,depression3$e12)
colnames(depression4,)
names(depression4)
depression5 <- subset(depression4,select = c(id1,a1,a4,a5,a6,a7,a8,a10,a11,a12,a13,b2,b3,b4,b9,b18,b58,a33,Objective_support
                                             ,a7_lable,Subjective_support,Support_utilization,f1,f2,f3,f4,f5,c1,e12))
colnames(depression5) <- c("Target type","Age","Type of social medical insurance","Commercial medical insurance","Marital status",
                           "Education level","Employment status","Main resource of income","Yearly individual total income",
                           "Yearly family avarage income","Family resident population","illness within 2 weeks","Hospitalization within 1 year",
                           "Gynecological diseases (yes or no)","Subjective health status","Sum of pregnancy complications","Sum of gynecological diseases",
                           "Period of time in Chengdu","Objective_support","education level for three class","Subjective_support","Support_utilization",
                           "Family adaptation","partnership","growth","affection","resolve","Actively acquire knowledge about pregnancy and perinatal period(yes or no)",
                           "depression")
depression5$`Gynecological diseases (yes or no)` <- temp
temp <- as.numeric(depression4$a6)
temp[which(temp)>2] <- 1

a <- summary(step(glm(depression~.,family = "binomial" ,data=depression5,na.action = na.omit)))
a$coefficients
fore <- read.table(pipe("pbpaste"),header=F,sep="\t")
fore1 <- read.table(pipe("pbpaste"),header=F,sep="\t")
fore$V10<- as.character(fore$V10)
fore$V9<- as.numeric(fore$V9)
fore <- fore[2:9,]
temp <- c(NA,"Estimate","Std.Error","Z value","Pr(>|z|)","low95%CI","high95%Cl",NA,"OR","OR(95%CI)")
fore1 <- rbind(temp,fore)
library("forestplot")
fore <- rbind(c(rep(NA,10)),fore)
forestplot(labeltext = fore1[,c(1,10,5,8)],
           mean=fore$V9,lower = fore$V6,upper = fore$V7,
           is.summary = c(T,F,F,F,F,F,F,F,F),
           zero=1,boxsize = 0.3,lineheight = 'auto',
           colgap = unit(2,'mm'),col = fpColors(box='skyblue',lines='black',zero= '#7AC5CD'),
           title = 'Fig.3 Logic analysis of multiple factors influencing maternal depression(n=452)',
           xlab = 'The estimates',
           txt_gp = fpTxtGp(ticks = gpar(familyfont="TimesNewRomanPSMT",cex = 1.25),
                            xlab = gpar(cex = 1), cex = 1),
           graph.pos = 2)
pdf("f.pdf",width = 40,height=40)
dev.off()
library("sjPlot","sjmisc")
start <- which(colnames(depression4)=="e1")
end <- which(colnames(depression4)=="e10")
EPDS <- depression4[,start:end]
temp <-  c("I have been able to laugh and see the funny side of things","I have looked forward with enjoyment to things",
"I have blamed myself unnecessarily when things went wrong","I have been anxious or worried for no good reason",
"I have felt scared or panicky for no very good reason","Things have been getting on top of me",
"I have been so unhappy that I have had difficulty sleeping","I have felt sad or miserable",
"I have been so unhappy that I have been crying","The thought of harming myself has occurred to me")
colnames(EPDS) <- temp
plot_stackfrq(EPDS,title = "Fig.1 Outcome of EPDS",
              legend.labels = c("never","sometimes","often","always"),
              wrap.labels = 60,geom.size = 0.6,
              show.prc = TRUE,grid.breaks = 0.1,vjust = "left")
table(depression4$a1)
summary(aov(depression~.,data = depression5))#定量做方差aov\var.test()，定性做卡方chisq.test()，单元格小于5用fisher.test
chisq.test(depression4$e12,depression4$id1)

table(depression4$c234)
library("ggplot2")
ggplot(infoway,aes(c211)) + geom_bar()
infoway <- subset(infoway,select = -c(c2110x,c225x,c235x))

x$
x1 <- c(308,276,180,163,137,122,184,259,143,282,114,118,115,270,254,204,176)
x2 <- c(rep("Prenatal",9),rep("Childbirth",4),rep("Postpartum",4))
x <- as.data.frame(cbind(x1,x2,x3))
x3 <- c("Fetal development", "Maternal nutrition ", "Maternal exercise","Weight control",
        "Emotional changes and regulation", "Identification and management of pregnancy complications",
  "Antenatal care", "Antenatal examination", "Drug use", "Delivery mod", "Delivery stage", "Parturition", "Medical choic", 
"Infant car", "Infant feeding", "Postpartum recovery", "Postpartum nutrition")

cols <- c(rgb(red =0, green = 0, blue = 128, max = 255),
          rgb(red =135, green =206, blue = 235, max = 255),
  rgb(red = 70, green = 130, blue = 180, max = 255))

colnames(x) <- c("x1","period","x3")
x$x3 = factor(x$x3, levels=c("Fetal development", "Maternal nutrition ", "Maternal exercise","Weight control",
                             "Emotional changes and regulation", "Identification and management of pregnancy complications",
                             "Antenatal care", "Antenatal examination", "Drug use", "Delivery mod", "Delivery stage", "Parturition", "Medical choic", 
                             "Infant car", "Infant feeding", "Postpartum recovery", "Postpartum nutrition"))

x$x3 <- as.factor(x$x3)
ggplot(data=x,mapping=aes(x=x3,y=x1,label=x1))+
  geom_text(nudge_y=0.3,angle=0)+
  geom_bar(stat="identity",aes(fill = period),position = 'dodge',width = 0.9)+
  scale_fill_manual(values = cols) +
  ylab("Count") +
  theme(axis.text.x =element_text(angle = 35, hjust = 1,colour="grey20", size=8),
        plot.title =element_text(size=15) )+
  labs(title ="Fig.2 Pregnancy related information")
pdf("m.pdf")
dev.off()

 # geom_bar(stat="identity",colour = "white"))
income <- subset(depression4,select = c(a11,a12,e12))

library(fBasics)

t.test(a1~e12,depression4,alternative="greater")
wilcox.test(f2~e12,depression4)
chisq.test(table(depression4$b18,depression4$e12))
fisher.test(table(depression4$a12,depression4$e12))
summary(aov(depression~.,depression5))
active_factor <- active_factor - depression4$e2
negative_factor <- rowSums(depression4[,c("e3","e4","e5","e6")])-4
physic_factor <- rowSums(depression4[,c("e7","e8","e9","e10")])-4
depression4$active_factor <- active_factor
depression4$negative_factor <- negative_factor
depression5$physic_factor <- physic_factor
CreateTableOne(vars = c("active_factor","negative_factor","physic_factor"), depression4,strata = c('e12'))

temp <-matrix(c(130,73,12,13),ncol=2,nrow = 2)
fisher.test(temp)
t.test(c(4.49,4.64))
chisq.test(temp)
str(depression4$b56)
summary(glm(e12~b15,depression4,family="binomial"),na.omit=T)
summary(depression4$e11)
sd(depression4$e11)
table(depression4$b9,depression4$e12)
edit(depression5)
#Objective_support,a7_lable,Subjective_support,Support_utilization