##  http://www.cnblogs.com/jpld/p/4594003.html  方差分析

x <- c(20,12,20,10,14,22,10,20,12,6,24,14,18,18,10,16,4,8,6,18,26,22,16,20,10)
a <- gl(5,5)
class(x)
length(x)
b <- gl(5,1,25)
b
 #gl(n, k, length=n*k,labels=1:n,ordered=FALSE)
 #n是因子的水平个数;k表示每一水平上的重复次数;
 #length=n*k表示总观测数;可通过参数labels对因子的不同水平添加标签;
 #ordered为逻辑值，指示是否排序。
 sales <- data.frame(x,a,b)
 sales
 ##1判断方差齐性
 bartlett.test(x~a)
 bartlett.test(x~b)#方差齐
 library(car)
 leveneTest(x~a)#==
 leveneTest(x,as.factor(a))
 #在新的R版本中 (我这里是3.1.3), levene.test()将会被删除
 #取而代之的是leveneTest()
 
 #然后, 格式是leveneTest(y, group)
 #y是你想检测homogeneity of variance 的所有数据， group给的是这些变量的分组信息。
 #所以后面一个变量会被当做factor处理
 
 #你这里如果想检测的是datasale和dataincome这两个变量的话
 #假设datascale=c(1,2,3,4,5)
 #dataincome=c(6,7,8,9,10)
 
 #你把他组成一个变量：y=c(datascale, dataincome)=c(1,2,3,4,5,6,7,8,9,10)
 #然后构造对应的分组变量：group=as.factor(c(1,1,1,1,1,   2,2,2,2,2))
 #就可以继续做方差齐性检验了：
 #leveneTest(y = y, group=group)
 #2画图
 interaction.plot(a,b,x,type = "o",lty = 1:5,legend = F)
 legend(x = "right",legend = c("b1","b2","b3","b4","b5"),lty = 1:5)
 interaction.plot(b,a,x,type = "o",lty = 1:5)
 legend(x = "right",legend = c("a1","a2","a3","a4","a5"),lty = 1:5)
 #3方差分析
  fit9 <- aov(x~factor(a))
 summary(fit9)
 anova(x~a*b)#anova（）需要一个模型对象，而不是公式，作为其第一个参数：
#anova(lm())
 mol <- aov(x~as.factor(a))
 Anova(mol,type=3)# type III 的结果才是我们常说的单因素方差分析的结果
 ?lm
 #4参数估计
 fit$coefficients
 #5多重比较
 pairwise.t.test(x,a)
 pairwise.t.test(x,b)

 #######7 方差分析p98  r 与统计##########
 ####单因子
 ex8   <- as.data.frame(example8_2)
 shapiro.test(ex8$产量)#正态性检验 n<=50,p>a  正态
 qqnorm(ex8$产量)#qq图 正态性
 qqline(ex8$产量)
 bartlett.test(产量~品种,data = example8_2) #方差齐性检验 p>a 方差齐
 fit <- with(ex8,aov(产量~品种))
summary(fit) #p<a 显著 品种对产量有显著影响
fit$coefficients #参数估计 品种1最好
library(gplots)# 均值图
plotmeans(with(ex8,产量~品种),cex =0.9,cex.axis = 0.7,lty = 1,cex.lab = 0.7)
pairwise.t.test(ex8$产量,ex8$品种) #多重比较 p<a 显著  品种12 23 显著区别
##双因子 无重复
shapiro.test(example8_4$产量) #p>a 正态
bartlett.test(with(example8_4,产量~品种))
bartlett.test(with(example8_4,产量~施肥方式))#p>a,方差齐
install.packages("multcomp")
library(multcomp)
fit2 <- aov(with(example8_4,产量~品种+施肥方式))
summary(fit2)# p<a,显著
fit2$coefficients#参数估计 品种1施肥方式甲最优
library(gplots)
plotmeans(with(example8_4,产量~施肥方式),n.label = F)
pairwise.t.test(example8_4$产量,example8_4$品种) #品种12 23有显著差异
##交互效应分析
fit3 <- aov(with(example8_4,产量~品种+施肥方式+品种:施肥方式))
summary(fit3) #p<a显著 品种与施肥方式之间交互作用不显著
fit3$coefficients #参数估计品种1 施肥方式乙最优
par(mfcol = c(1,1))# 面板中可以话一行两列图
interaction.plot(example8_4$品种,example8_4$施肥方式,example8_4$产量,
                 type = "o",lty = 1:2,ylab = "产量均值",xlab = "品种",
                 cex = 0.9,legend = F)
legend(lty=1:2,cex = 0.7,x = "top",legend = c("施肥方式甲","施肥方式乙"))

interaction.plot(example8_4$施肥方式,example8_4$品种,example8_4$产量,
                 lty = 1:3,ylab = " 量产均值",xlab  = "施肥方式",legend = F,type = "o")
legend(lty = 1:3,x = "topleft",cex = 0.7,legend = c(" 品种1","品种2","品种3"))


#练习 p111 7.1
bartlett.test(装填量~机器,exercise8_1) #方差分析 p>2 方差齐
fit4 <- aov(装填量~机器,data= exercise8_1)
summary(fit4) #p<0.01 显著
fit4$coefficients #参数估计 机器1最优
plotmeans(装填量~机器,data= exercise8_1,lty = 1)
pairwise.t.test(exercise8_1$装填量,exercise8_1$机器) #机器13 差异显著

#7.2
bartlett.test(评分~管理者类型,exercise8_2)
fit5 <- aov(评分~管理者类型,exercise8_2) #方差分析
summary(fit5)
fit5$coefficients #参数估计
plotmeans(评分~管理者类型,exercise8_2,main = "管理者水平不同对评分的影响")
pairwise.t.test(exercise8_2$评分,exercise8_2$管理者类型) #低高 低中有显著差异


#7.3
bartlett.test(使用寿命~企业,exercise8_3)
fit6 <- aov(使用寿命~企业,exercise8_3)
summary(fit6)
fit6$coefficients
plotmeans(使用寿命~企业,exercise8_3)
pairwise.t.test(exercise8_3$使用寿命,exercise8_3$企业) #AB bc


###双因子方差分析???########
load("E:/DATAS/example/ch7/example8_4.RData")
bartlett.test(产量~品种,exercise8_4)
bartlett.test(产量~施肥方式,exercise8_4)
fit7 <- aov(产量~as.factor(施肥方式)*as.factor(品种),exercise8_4)
summary(fit7)#????????????df=1     需要as.factor
fit7$coefficients#参数估计? as.factor以后多水平
par(mfcol=c(1,1))
interaction.plot(exercise8_4$品种,exercise8_4$施肥方式,exercise8_4$产量,
                 type = "o",lty = 1:4,ylab = "产量均值",xlab = "品种",
                 cex = 0.9,legend = F)
legend(lty=1:4,cex = 0.55,x = "topleft",legend = c("施肥方式1","施肥方式2","施肥方式3","施肥方式4"))
interaction.plot(exercise8_4$施肥方式,exercise8_4$品种,exercise8_4$产量,
                 lty = 1:5,ylab = " 量产均值",xlab  = "施肥方式",legend = F,type = "o")
legend(lty = 1:5,x = "topright",cex = 0.7,legend = c(" 品种1","品种2","品种3","品种4","品种5"))


#7.4
load("E:/DATAS/exercise/ch8/exercise8_5.RData")
View(exercise8_5)
bartlett.test(行车时间~路段,exercise8_5)
bartlett.test(行车时间~时段,exercise8_5)
fit8 <- aov(行车时间~路段*时段,exercise8_5)
summary(fit8)#交互不显著
fit8$coefficients #路段2非高峰时段时间最短,路段1高峰时段时间最长.
interaction.plot(exercise8_5$路段,exercise8_5$时段,exercise8_5$行车时间,lty =1:2 )
interaction.plot(exercise8_5$时段,exercise8_5$路段,exercise8_5$行车时间,lty =1:3 )#交互不显著
#7.5
load("E:/DATAS/exercise/ch8/exercise8_6.Rdata")
View(exercise8_6)
bartlett.test(销售量~广告媒体,exercise8_6)
bartlett.test(销售量~广告方案,exercise8_6)
interaction.plot(exercise8_6$广告媒体,exercise8_6$广告方案,exercise8_6$销售量,lty = 1:3,legend = F,type = "o")
interaction.plot(exercise8_6$广告方案,exercise8_6$广告媒体,exercise8_6$销售量,lty = 1:2,type = "o")
fit8 <- aov(销售量~广告媒体*广告方案,exercise8_6)
summary(fit8)
fit8$coefficients #参数估计
pairwise.t.test(exercise8_6$销售量,exercise8_6$广告方案)#ab有显著差异
pairwise.t.test(exercise8_6$销售量,exercise8_6$广告媒体)
