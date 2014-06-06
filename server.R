downloadNotInstalled<-function(x){
    for(i in x){
	if(!require(i,character.only=TRUE)){
	 install.packages(i,repos="http://cran.r-project.org")
  	 library(i,character.only=TRUE)
  	}
    }
}
requiredPackages = c("shiny","vegan")
downloadNotInstalled(requiredPackages)

if(!require("metagenomeSeq")){
	source("http://bioconductor.org/biocLite.R")
	biocLite("metagenomeSeq")
	library("metagenomeSeq")
}

mypar<-function(a=1,b=1,brewer.n=8,brewer.name="Dark2",...){
  par(mar=c(3.0,2.5,1.6,1.1),mgp=c(1.5,.5,0))
  par(mfrow=c(a,b),...)
  palette(brewer.pal(brewer.n,brewer.name))
}

load("forserveroptim.rdata")

status = factor(pData(gates)$Type)
country = factor(pData(gates)$Country)
agefactor = factor(pData(gates)$AgeFactor)
os = paste(fData(gates)[,1],fData(gates)[,"species"],sep=":")

shinyServer(function(input, output) {
  output$plot <- renderPlot({
    samplesToInclude = which(country%in%input$country & agefactor%in%input$age)
    subset = gates[,samplesToInclude]

    if(sum(input$strata%in%"none")){
      dim2 = dim1 = 1
      clIndex = list(all_samples = 1:ncol(subset))
    } else {
      clIndex = interaction(pData(subset)[,input$strata])
      nms = levels(clIndex)
      clIndex = lapply(levels(clIndex),function(i){which(clIndex==i)})
      names(clIndex) = nms
      dim1 = dim2 = ceiling(length(clIndex)/2)

      if(dim1*dim2 < length(clIndex)){ dim1= dim1+1}
      while(dim1*dim2 > length(clIndex)){
        dim1=dim1-1;
      }
      if(dim1*dim2 < length(clIndex)){ dim1= dim1+1}
      if(length(clIndex)>10){
        dims = n2mfrow(length(clIndex))
        dim1=dims[1]; dim2=dims[2]
      }
    }

    if(sum(input$strata%in%c("AgeFactor","Type","Country"))==3){
      coll = pData(subset)[,"Type"]
    } else if(sum(input$strata%in%c("AgeFactor","Type"))==2){
      coll = pData(subset)[,"Country"]
    } else if(sum(input$strata%in%c("Country","Type"))==2){
      coll = pData(subset)[,"AgeFactor"]
    } else{
      coll = pData(subset)[,"Type"]
    }

    if(input$level!="OTU"){
      if(input$level == 'genus'){
        inputFeature=input$genus
        if(input$norm == TRUE){
          mat = gnorm
        } else{
          mat = graw
        }
      } else if (input$level == 'species'){
        inputFeature=input$species
        if(input$norm == TRUE){
          mat = snorm
        } else{
          mat = sraw
        }        
      } else if (input$level == 'class') {
        inputFeature=input$class
        if(input$norm == TRUE){
          mat = cnorm
        } else{
          mat = craw
        }        
      } else if (input$level == 'phylum') {
        inputFeature=input$phylum
        if(input$norm == TRUE){
          mat = pnorm
        } else{
          mat = praw
        }
      }
      main   = rownames(mat)[inputFeature]
    } else {
      if(input$norm == TRUE){
        mat = nmat[,samplesToInclude]
      } else {
        mat = MRcounts(subset)
      }
    
      if(input$otu == TRUE){
        inputFeature = input$feature    
      } else{
        k = which(input$feature == fData(gates)[,1])
        inputFeature = k
      }
      main   = os[inputFeature]
    }
      
    mypar(dim1,dim2)
    plotFeature(mat,otuIndex = inputFeature,ylab="Abundance",main=main,
      classIndex = clIndex,col=coll,font.lab=2,font.axis=2)
    legend("topleft",legend=unique(coll),fill=unique(coll),box.col="NA")
  })

  output$pcaPlot <- renderPlot({

    if(input$pcaSamples!="Both"){
      samplesToInclude = which(status%in%input$pcaSamples)
      subset = gates[,samplesToInclude]
    } else {
      samplesToInclude = 1:ncol(nmat)
      subset = gates
    }
    useDist = input$useDist
    pd = pData(subset)[,input$pcaColor]
    if(input$pcaColor=="Type" | input$pcaColor=="Dysentery") pd = factor(pd)
    # if(input$pcaOrMds=="FALSE") useDist = TRUE
    plotOrd(nmat[,samplesToInclude],n=200,pch=21,bg=pd,usePCA=input$pcaOrMds,
      useDist=useDist,distfun=vegan::vegdist,dist.method=input$distance)
    legend("bottomleft",levels(pd),fill=factor(levels(pd)),box.col="NA")
  })

  output$diversity <- renderPlot({
    pd = pData(gates)[,input$comp]
    boxplot(H~interaction(pd))
  })

  output$diversityTable <- renderTable({
    pd = interaction(pData(gates)[,input$comp])
    nc = length(unique(pd))

    muH =as.vector(by(H,pd,mean))
    sdH =by(H,pd,sd)

    divs = rbind(muH,sdH)
    rownames(divs) = c("Mean","SD")
    colnames(divs) = levels(pd)
    divs
  })

  output$summary <- renderTable({
    samplesToInclude = which(country%in%input$country & agefactor%in%input$age)
    subset = gates[,samplesToInclude]

    if(sum(input$strata%in%"none")){
      x = table(pData(subset)$Type)
    } else {
      x = table(pData(subset)[,c(input$strata)])
    }

  })
  
  output$table <- renderTable({
    if(input$otu == TRUE){
      Index = input$feature
    } else{
      k = which(input$feature == fData(gates)[,1])
      Index = k
    }
    data.frame(Index, fData(gates)[Index,-c(2,3,10)])
  })
  output$clusterSequence<-renderText({
    if(input$otu == TRUE){
      Index = input$feature
    } else{
      k = which(input$feature == fData(gates)[,1])
      Index = k
    }
    otuid = paste(">",as.character(fData(gates)[Index,1]),sep="")
    seq = as.character(fData(gates)[Index,10])
    sprintf("%s\n%s",otuid,seq)
  })

  output$clusterSequences<-renderText({
      if(input$level == 'genus'){
        inputFeature=input$genus
      } else if (input$level == 'species'){
        inputFeature=input$species
      } else if (input$level == 'class'){
        inputFeature=input$class
      } else if (input$level == 'phylum'){
        inputFeature=input$phylum
      }
    k = which(fData(gates)[,input$level]==inputFeature)
    otuids = paste(sprintf(">%s",fData(gates)[k,1]),"\n",sep="")
    head(otuids)
    seqs = as.character(fData(gates)[k,10])
    paste(otuids,seqs,collapse="\n")
  })

  output$otulist<- renderDataTable({
        as.matrix(fData(gates)[,-c(2,3)])
    })
  
})
