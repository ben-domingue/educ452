
## https://www.oecd.org/pisa/data/2018database/


## cognitive item data file, https://webfs.oecd.org/pisa2018/SPSS_STU_COG.zip
## timing file, https://webfs.oecd.org/pisa2018/SPSS_STU_TIM.zip


## items come from cog/pisa_ms_cog_overall_math_compendium.xlsx

#################################################################################
## ##items
## it<-read.csv("items.csv",header=FALSE)
## index<-grep("Scored Response",it[,2])
## items<-it[index,1]
## dump("items","")

## items <-
## c("CM033Q01S", "CM474Q01S", "CM155Q01S", "CM155Q04S", "CM411Q01S", 
## "CM411Q02S", "CM803Q01S", "CM442Q02S", "CM034Q01S", "CM305Q01S", 
## "CM496Q01S", "CM496Q02S", "CM423Q01S", "CM192Q01S", "CM603Q01S", 
## "CM571Q01S", "CM564Q01S", "CM564Q02S", "CM447Q01S", "CM273Q01S", 
## "CM408Q01S", "CM420Q01S", "CM446Q01S", "CM559Q01S", "CM828Q03S", 
## "CM464Q01S", "CM800Q01S", "CM982Q01S", "CM982Q02S", "CM982Q03S", 
## "CM982Q04S", "CM992Q01S", "CM992Q02S", "CM915Q01S", "CM915Q02S", 
## "CM906Q01S", "CM909Q01S", "CM909Q02S", "CM909Q03S", "CM949Q01S", 
## "CM949Q02S", "CM00GQ01S", "CM955Q03S", "CM998Q04S", "CM905Q01S", 
## "CM919Q01S", "CM919Q02S", "CM954Q01S", "CM954Q04S", "CM943Q01S", 
## "CM943Q02S", "CM953Q03S", "CM948Q01S", "CM948Q02S", "CM948Q03S", 
## "CM936Q01S", "CM961Q03S", "CM939Q01S", "CM939Q02S", "CM967Q01S", 
## "CM967Q03S", "PM033Q01S", "PM155Q04S", "PM411Q02S", "PM803Q01S", 
## "PM034Q01S", "PM305Q01S", "PM496Q01S", "PM423Q01S", "PM192Q01S", 
## "PM603Q01S", "PM571Q01S", "PM564Q01S", "PM564Q02S", "PM447Q01S", 
## "PM273Q01S", "PM408Q01S", "PM420Q01S", "PM559Q01S", "PM464Q01S", 
## "PM800Q01S", "PM982Q03S", "PM982Q04S", "PM915Q01S", "PM906Q01S", 
## "PM909Q02S", "PM949Q01S", "PM949Q02S", "PM998Q04S", "PM905Q01S", 
## "PM943Q01S", "PM948Q01S", "PM961Q03S", "PM939Q01S", "PM939Q02S", 
## "PM967Q03S")

#################################################################################

library(foreign)
options(warn=-10)
dat<-read.spss("CY07_MSU_STU_COG.sav",
              to.data.frame=TRUE,
              use.value.labels=FALSE
              )
options(warn=1)

index<-sample(1:nrow(dat),50000)
dat<-dat[index,]

toc<-c(math="pisa_ms_cog_overall_math_compendium_TOC.csv")#,read="pisa_ms_cog_overall_read_compendium_TOC.csv")
for (iii in 1:length(toc) ) {
    dat->x
    it<-read.csv(toc[iii],header=FALSE)
    index<-grep("Scored Response",it[,2])
    items<-it[index,1]
    print(length(items))

    resp<-x[,items]
    nn<-apply(resp,2,function(x) length(unique(x[!is.na(x)])))
    resp<-resp[,nn==2]

    nms<-names(resp)
    for (i in 1:length(nms)) {
        nm<-nms[i]
        n<-nchar(nm)
        nm<-substr(nm,1,n-1)
        nms[i]<-nm
    }
    nms2<-paste(nms,"T",sep="")
    test<-nms2 %in% names(x)
    resp<-resp[,test]
    ## rt<-x[,nms2[test]]

    ## for (i in 1:ncol(rt)) {
    ##     z<-rt[,i]
    ##     z<-as.numeric(z)
    ##     z<-ifelse(z<=0,NA,z)
    ##     log(z/1000)->rt[,i]
    ## }

    ##get rid of empty rows
    rs<-rowSums(is.na(resp))
    test<-rs<ncol(resp)
    resp<-resp[test,]
    ##rt<-rt[test,]
    
    
    ##create long data
    id<-1:nrow(resp)
    item<-names(resp)
    L<-list()
    for (i in 1:ncol(resp)) {
        L[[i]]<-data.frame(resp=resp[,i],id=id,item=item[i])
    }
    x<-data.frame(do.call("rbind",L))
    
    
    rs<-rowSums(is.na(x))
    x<-x[rs==0,]
    
    save(x,file=paste0("pisa2018",names(toc)[iii],"_df.Rdata"))
}
