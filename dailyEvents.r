library("ggplot2")

mydata <- read.csv("/Users/sciruela/Documents/dailyEvents/syria.csv")

class(mydata$Number)<-"numeric"

mydata$ttime=as.POSIXct(strptime(mydata$History, "%d/%m/%Y"),tz='UTC')

myplot=subset(mydata,select=c('ttime','Number'))
myplot=arrange(myplot,-desc(ttime))
mydata$Number=factor(mydata$Number, levels = myplot$Number)


pdf("/Users/sciruela/Documents/dailyEvents/syria1.pdf")
ggplot(mydata) + geom_point(aes(x=ttime,y=Counter)) + opts(axis.text.x=theme_text(size=6),axis.text.y=theme_text(size=4))
dev.off()

pdf("/Users/sciruela/Documents/dailyEvents/syria2.pdf")
ggplot(mydata) + geom_line(aes(x=ttime,y=Counter,col='Syria Events')) + labs(colour='Type') + xlab(NULL)
dev.off()

pdf("/Users/sciruela/Documents/dailyEvents/syria3.pdf")
ggplot(mydata) + geom_linerange(aes(x=ttime,ymin=0,ymax=Friday,col="Friday")) + geom_linerange(aes(x=ttime,ymin=0,ymax=NoFriday,col="No Friday")) + opts(axis.text.x=theme_text(angle=-90,size=6)) + xlab(NULL)
dev.off()

class(mydata$Number)<-"numeric"
means<-tapply(mydata$Number,mydata$WeekDay,mean)

pdf("/Users/sciruela/Documents/dailyEvents/syria4.pdf")
ggplot(mydata)+geom_bar(aes(x=unique(WeekDay),y=means,colors=unique(WeekDay),fill=unique(WeekDay)),binwidth=1)+theme_bw()+xlab("")
dev.off()

pdf("/Users/sciruela/Documents/dailyEvents/syria5.pdf")
ggplot(mydata,aes(WeekDay,Number))+geom_boxplot(aes(colors=mydata$WeekDay,fill=mydata$WeekDay))+theme_bw()+xlab("")
dev.off()

densities<-tapply(mydata$Number,mydata$WeekDay,density)

pdf("/Users/sciruela/Documents/dailyEvents/syria6.pdf")
ggplot(mydata,aes(WeekDay,Number))+geom_boxplot(aes(colors=mydata$WeekDay,fill=mydata$WeekDay))+theme_bw()+xlab("")
dev.off()

pdf("/Users/sciruela/Documents/dailyEvents/syria7.pdf")
ggplot(mydata,aes(WeekDay,log(Number)))+geom_boxplot(aes(colors=mydata$WeekDay,fill=mydata$WeekDay))+theme_bw()+xlab("")
dev.off()


pdf("/Users/sciruela/Documents/dailyEvents/syria8.pdf")
ggplot(mydata) +geom_density(aes(x=log(mydata$Friday),alpha=.8, colour="Friday", fill="Friday"))+geom_density(aes(x=log(mydata$NoFriday),alpha=.8, colour="NoFriday", fill="NoFriday"))+xlab("")
dev.off()

pdf("/Users/sciruela/Documents/dailyEvents/syria9.pdf")
ggplot(mydata) +geom_density(aes(x=log(mydata$Friday),alpha=.8, colour="Friday", fill="Friday"))+geom_density(aes(x=log(mydata$Monday),alpha=.8, colour="Monday", fill="Monday"))+geom_density(aes(x=log(mydata$Tuesday),alpha=.8, colour="Tuesday", fill="Tuesday"))+geom_density(aes(x=log(mydata$Wednesday),alpha=.8, colour="Wednesday", fill="Wednesday"))+geom_density(aes(x=log(mydata$Thursday),alpha=.8, colour="Thursday", fill="Thursday"))+geom_density(aes(x=log(mydata$Saturday),alpha=.8, colour="Saturday", fill="Saturday"))+geom_density(aes(x=log(mydata$Sunday),alpha=.8, colour="Sunday", fill="Sunday"))+xlab("")
dev.off()



library(ggplot2)

input=read.csv("http://dl.dropbox.com/u/1391912/Blog%20statisfaction/data/syria.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE)

input$LogicalFriday=factor(input$WeekDay =="Friday",levels = c(FALSE, TRUE),

labels = c("Not Friday", "Friday"))

input$Date=as.Date(input$History,"%d/%m/%Y")

input$WeekDays=factor(input$WeekDay,
levels=unique(as.character(input$WeekDay[7:13]))) # trick to sort the legend
pdf("/Users/sciruela/Documents/dailyEvents/syria-original1.pdf")
qplot(x=Date,y=cumsum(Number), data=input, geom="line",color=I("red"),xlab="",ylab="",lwd=I(1))
dev.off()
pdf("/Users/sciruela/Documents/dailyEvents/syria-original2.pdf")
qplot(x=as.factor(Date),y=Number, data=input, geom="bar",fill=LogicalFriday,xlab="",ylab="")
dev.off()
pdf("/Users/sciruela/Documents/dailyEvents/syria-original3.pdf")
qplot(log(Number+1), data=input, geom="density",fill=LogicalFriday,xlab="",ylab="",alpha=I(.2))
dev.off()
pdf("/Users/sciruela/Documents/dailyEvents/syria-original4.pdf")
qplot(log(Number+1), data=input, geom="density",fill=WeekDay,xlab="",ylab="",alpha=I(.2))
dev.off()
pdf("/Users/sciruela/Documents/dailyEvents/syria-original5.pdf")
qplot(WeekDays,log(Number+1),data=input,geom="boxplot",xlab="",ylab="",colour=WeekDays)
dev.off()