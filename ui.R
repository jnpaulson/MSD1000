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

googleAnalytics <- function(account="UA-51743143-1"){
  HTML(paste("<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', '",account,"', 'umd.edu');
  ga('send', 'pageview');

</script>", sep=""))
}

load("forserveroptim.rdata")

headerInfo = tags$h3("Hosted by:",tags$a(tags$img(src="logo.png"),href="http://epiviz.cbcb.umd.edu"),
                p(),tags$h6("Data comes from the ",
                  tags$a("molecular characterization of the diarrheal microbiome in young children from low-income countries",
                    href="http://www.cbcb.umd.edu/research/projects/GEMS-pathogen-discovery")))
footerInfo = tags$small(tags$a("Visualization code",href="https://github.com/nosson/MSD1000"))
#," by ",tags$a("Joseph N. Paulson",href="http://www.cbcb.umd.edu/~jpaulson")),

shinyUI(navbarPage("MSD 1000",
  header = headerInfo,
  footer = footerInfo,
	tabPanel("Feature Abundance plots",
        sidebarLayout(
          sidebarPanel(
            radioButtons("level","Level:",c(
                        "Phylum" = "phylum",
                        "Class" = "class",
                        "Genus" = "genus","Species"="species","OTU"="OTU"),
                        selected = c("OTU")),
              conditionalPanel(condition = "input.level == 'OTU'",
                checkboxInput("otu","Index",TRUE),
                numericInput('feature', 'OTU:', 1, min = 1, max = 197358,value=3712)
              ),
              conditionalPanel(condition = "input.level == 'species'",
                selectInput('species', 'Bacteria:', rownames(sraw))
              ),
              conditionalPanel(condition = "input.level == 'genus'",
                selectInput('genus', 'Bacteria:', rownames(graw))
              ),
              conditionalPanel(condition = "input.level == 'class'",
                selectInput('class', 'Bacteria:', rownames(craw))
              ),
              conditionalPanel(condition = "input.level == 'phylum'",
                selectInput('phylum', 'Bacteria:', rownames(praw))
              ),
            checkboxInput("norm", "Normalize", TRUE),
            br(),
            checkboxGroupInput("country","Country:",c(
                        "Kenya" = "Kenya",
                        "The Gambia" = "Gambia",
                        "Mali" = "Mali","Bangladesh"="Bangladesh"),
                        selected = c("Kenya","Gambia","Mali","Bangladesh")),
            checkboxGroupInput("age","Age:",c(
                        "[0,6)" = "[0,6)",
                        "[6,12)" = "[6,12)",
                        "[12,18)" = "[12,18)","[18,24)"="[18,24)","[24,60)"="[24,60)"),
                        selected = c("[0,6)","[6,12)","[12,18)","[18,24)","[24,60)")),
            br(),
            checkboxGroupInput("strata","Stratified by:",c(
                        "Country" = "Country",
                        "Age" = "AgeFactor",
                        "Health" = "Type", "None" = "none"),
                        selected = c("Type","AgeFactor")),
            br(),
            conditionalPanel(condition = "input.level != 'OTU'",
              tags$small("*OTU sequence centers for OTUs present in > 20 samples")
              )
          ),
          mainPanel(
            plotOutput("plot"),
            conditionalPanel(condition = "input.level == 'OTU'",
              tableOutput("table"),
              pre("OTU sequence center:",textOutput("clusterSequence"))
            ),
            conditionalPanel(condition = "input.level != 'OTU'",
              pre("*OTU sequence centers:",textOutput("clusterSequences"))
            ),
            googleAnalytics()
          )
      )
    ),
  tabPanel("PCA",
        sidebarLayout(
          sidebarPanel(
            radioButtons("pcaSamples","Samples to use:",c("Both"="Both","Cases"="Case","Controls"="Control")),
            radioButtons("pcaOrMds","PCA or MDS:",c("PCA"="TRUE","MDS"="FALSE")),
            radioButtons("useDist","Count distances:",c("False"="FALSE","True"="TRUE")),
              conditionalPanel(condition = "input.useDist == 'TRUE'",
                selectInput("distance", "Distance:", 
                    choices=c("euclidean","manhattan","canberra","bray",
                      "kulczynski","jaccard","gower","altGower","morisita",
                      "horn","mountford","raup","binomial","chao","cao"))
              ),
            radioButtons("pcaColor","Colored by:",c(
                        "Country" = "Country",
                        "Age" = "AgeFactor",
                        "Health" = "Type",
                        "Dysentery"="Dysentery"),
                        selected = c("Country")),
            br(),
            tags$small("Warning: takes a few seconds")
          ),
          mainPanel(
            plotOutput("pcaPlot")
          )
      )
    ),
    tabPanel("Diversity",
      sidebarLayout(sidebarPanel(
        checkboxGroupInput("comp",
            "Comparisons:",c("Country"="Country","Age" = "AgeFactor","Health"="Type","Dysentery"="Dysentery"),selected=c("Type"))
        ),
      mainPanel(plotOutput("diversity"),tableOutput("diversityTable"))))
    ,
    tabPanel("Rarefaction",
      sidebarLayout(sidebarPanel(
        checkboxGroupInput("rare",
            "Comparisons:",c("Country"="Country","Age" = "AgeFactor","Health"="Type","Dysentery"="Dysentery"),selected=c("Type"))
        ),
      mainPanel(plotOutput("plotRare"))))
    ,    
    navbarMenu("More",
      tabPanel("OTU descriptions",
            mainPanel(
                dataTableOutput("otulist")
             )
          ),
      tabPanel("About",
        tags$h6(
    "Diarrhea is a major cause of mortality and morbidity in young children from developing countries, leading to as many as 15% of all deaths in children under 5 years of age. While many causes of this disease are already known, conventional diagnostic approaches fail to detect a pathogen in up to 60% of diarrheal cases. Our study is part of the larger Global Enterics Multi-center study (GEMS) and aims to characterize the diarrheal microbiome in order to both evaluate the effectiveness of modern diagnostics based on molecular techniques, and to discover potentially new pathogens.",p(),
    "Among the discoveries of our study are the effectiveness of quantitative PCR as an alternative to culture in characterizing Shigella infections, as well as the potential of members of the Streptococcus genus to cause diarrhea. Streptococci were found in our study to be statistically associated with diarrheal disease in general and more severe forms (such as dysentery) in particular.",p(),
    "Data was filtered (for server speed) at each level to include only features present in over 20 samples. Additionally, only OTU sequences present in over 20 samples are shown."
),p(),tags$small(tags$a("Visualization code",href="https://github.com/nosson/MSD1000")," by ",tags$a("Joseph N. Paulson",href="http://www.cbcb.umd.edu/~jpaulson")),p(),p()
          )
         )
	)
)
