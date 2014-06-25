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
source("plotMRheatmap2.R")

fixLabels = function(x){
  if("Gambia"%in%x){
    x = as.character(x)
    x[x=="Gambia"] = "The Gambia"
    x = factor(x)
    return(x)
  } else {
    return(x)
  }
}

status = factor(pData(gates)$Type,levels=c("Control","Case"))
country = pData(gates)$Country
country = fixLabels(country)
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
    coll = fixLabels(coll)

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
      main   = inputFeature
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
    ylabel = ifelse(input$norm,yes="Abundance log2(cpt)",no="No. raw reads")
    mypar(dim1,dim2)
    plotFeature(mat,otuIndex = inputFeature,ylab=ylabel,main=main,
      classIndex = clIndex,col=coll,font.lab=2,font.axis=2,sort=FALSE)
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
    pd = fixLabels(pData(subset)[,input$pcaColor])
    # if(input$pcaOrMds=="FALSE") useDist = TRUE
    plotOrd(nmat[,samplesToInclude],n=200,pch=21,bg=pd,usePCA=input$pcaOrMds,
      comp=c(input$dimensionx,input$dimensiony),
      useDist=useDist,distfun=vegan::vegdist,dist.method=input$distance)
    legend("bottomleft",levels(pd),fill=factor(levels(pd)),box.col="NA")
  })

  output$diversity <- renderPlot({
    pd = fixLabels(pData(gates)[,input$comp])
    if("Country"%in%colnames(pd)){
      pd2 = pd[,"Country"]
      pd2 = fixLabels(pd2)
      pd[,"Country"] = pd2
    }
    boxplot(H~interaction(pd),ylab="Shannon diversity index")
  })

  output$diversityTable <- renderTable({
    pd = fixLabels(pData(gates)[,input$comp])
    if("Country"%in%colnames(pd)){
      pd2 = pd[,"Country"]
      pd2 = fixLabels(pd2)
      pd[,"Country"] = pd2
    }    
    pd = interaction(pd)
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
    if(inputFeature=="no_match"){
      k = which(is.na(fData(gates)[,3]))
    }
    otuids = paste(sprintf(">%s",fData(gates)[k,1]),"\n",sep="")
    seqs = as.character(fData(gates)[k,10])
    paste(otuids,seqs,collapse="\n",sep="")
  })

  output$plotRare<-renderPlot({
    if(is.null(input$rare)){
      cl = "black"
    } else {
      cl = fixLabels(pData(gates)[,input$rare])
      if("Country"%in%colnames(cl)){
        cl2 = cl[,"Country"]
        cl2 = fixLabels(cl2)
        cl[,"Country"] = cl2
      }
      cl = interaction(cl)
      if("Dysentery"%in%input$rare){ cl = factor(cl)}
    }
    plot(totalCounts, numFeatures, xlab = "Depth of coverage", 
            ylab = "Number of detected features",col='grey',bg=cl,pch=21)

    if(!is.null(input$rare)){
      legend("topleft",legend=levels(cl),fill=factor(levels(cl)),box.col="NA")
      tmp = lapply(levels(cl), function(lv) lm(numFeatures~totalCounts-1, subset=cl==lv))
      for(i in 1:length(levels(cl))){
          abline(tmp[[i]], col=i)
      }
      ar2 = paste("Adj. R^2: ",round(summary(lm(numFeatures~totalCounts-1+cl))$adj,digits=3))
    } else {
      ar2 = paste("Adj. R^2: ",round(summary(lm(numFeatures~totalCounts-1))$adj,digits=3))
    }
      legend("bottomright", legend=ar2,box.col=NA)    
  })

  output$plotHeatmap<-renderPlot({
    if(input$heatSamples!="Both"){
      samplesToInclude = which(status%in%input$heatSamples)
      subset = nmat[,samplesToInclude]
      gatesSubset = gates[,samplesToInclude]
    } else {
      samplesToInclude = 1:ncol(nmat)
      subset = nmat
      gatesSubset = gates[,samplesToInclude]
    }
    trials = fixLabels(pData(gatesSubset)[,input$heatColumns])
    heatmapColColors=brewer.pal(12,"Set3")[as.integer(trials)];
    heatmapCols = colorRampPalette(brewer.pal(9, "RdBu"))(50)
    rownames(subset) = paste(fData(gatesSubset)[,"species"],fData(gatesSubset)[,"OTU"],sep=":")
    plotMRheatmap2(subset,n=input$heatNumber,fun=input$heat,
              cexRow = 0.25,cexCol=0.4,trace="none",
              dendrogram="column", key=TRUE,
              lwid=c(1,4), lhei=c(1,4),
              margins=c(2,2),
              col = heatmapCols,ColSideColors = heatmapColColors)
    legend("left",fill=unique(heatmapColColors),legend=levels(trials))

  })
  output$otulist<- renderDataTable({
        as.matrix(fData(gates)[,-c(2,3)])
  })
  
})
