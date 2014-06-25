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
                p(),tags$h4("Data comes from the ",
                  tags$a("molecular characterization of the diarrheal microbiome in young children from low-income countries",
                    href="http://www.cbcb.umd.edu/research/projects/GEMS-pathogen-discovery")))
footerInfo = tags$small(tags$a("Visualization code",href="https://github.com/nosson/MSD1000"))

shinyUI(navbarPage("MSD 1000",
  # header = headerInfo,
  # footer = footerInfo,
  tabPanel("About",tags$h3("Interactive visualization explorer of the diarrheal microbiome in young children from low-income countries"),p("Diarrhea is a major cause of mortality and morbidity in young children from developing countries, leading to as many as 15% of all deaths in children under 5 years of age."),
    p("Among the discoveries of our study are the effectiveness of quantitative PCR as an alternative to culture in characterizing Shigella infections, as well as the potential of members of the Streptococcus genus to cause diarrhea. The data underlying our study is presented in part through these interactive plots."),
  tags$hr(),tags$h3("Explore the data!"),p("We welcome you to explore the data on this site through the tabs above. Data was filtered (for server speed) at each level to include only bacteria present in over 20 samples. Additionally, only OTU sequences present in over 20 samples are shown."),
  tags$h5("Bacterial Abundance"),p("Abundance of bacteria at various levels of the tree stratified by sample health status, age, and country of origin. Scroll through your favorite bacteria and display your country/age of interest."),
  tags$h5("Heatmap"),p("Heatmap of the bacterial abundances filtered by either the most variable bacteria or those with the largest median absolute deviation scores.",strong("warning: takes a few seconds")),
  tags$h5("PCA/MDS"),p("PCA or MDS plots of the samples",strong("warning: takes a few seconds")),
  tags$h5("Diversity"),p("Shannon diversity index boxplots of the various stratified samples."),
  tags$h5("Rarefaction"),p("Operational taxonomic unit (OTU) richness as a function of sequencing depth."),
  tags$h5("OTU Description"),p("OTU representative sequence annotations."),
  tags$hr(),headerInfo,
  tags$small(tags$a("Visualization code",href="https://github.com/nosson/MSD1000")," by ",tags$a("Joseph N. Paulson",href="http://www.cbcb.umd.edu/~jpaulson"))),

	tabPanel("Bacterial Abundance",
    tags$h6("Manhattan plots of bacterial abundances where each bar represents the abundance in a sample."),
        sidebarLayout(
          sidebarPanel(
            selectInput("level","Level:",c(
                        "Phylum" = "phylum",
                        "Class" = "class",
                        "Genus" = "genus","Species"="species","OTU"="OTU"),
                        selected = c("species")),
              conditionalPanel(condition = "input.level == 'OTU'",
                checkboxInput("otu","Index",TRUE),
                numericInput('feature', 'OTU:', 1, min = 1, max = 197358,value=3712)
              ),
              conditionalPanel(condition = "input.level == 'species'",
                selectInput('species', 'Bacteria:', rownames(sraw),selected="Streptococcus mitis")
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
            checkboxGroupInput("country","Include country:",c(
                        "Kenya" = "Kenya",
                        "The Gambia" = "The Gambia",
                        "Mali" = "Mali","Bangladesh"="Bangladesh"),
                        selected = c("Kenya","The Gambia","Mali","Bangladesh")),
            checkboxGroupInput("age","Include age (months):",c(
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
  tabPanel("Heatmap",
      tags$h6("Heatmap of the top N bacteria."),
      sidebarLayout(sidebarPanel(
        radioButtons("heatSamples","Samples to use:",c("Both"="Both","Cases"="Case","Controls"="Control"),selected="Control"),
        radioButtons("heat","Choose bacteria by:",c("Median Absolute Deviation"="mad","Variability"="sd")),
        numericInput('heatNumber', 'Number of bacteria to display:', 15,
                 min = 5, max = 400),
        radioButtons("heatColumns",
            "Column labels:",c("Health"="Type","Country"="Country","Age" = "AgeFactor","Dysentery"="Dysentery"),selected="AgeFactor"),
        br(),
        tags$small("Warning: takes a few seconds")
        ),
      mainPanel(plotOutput("plotHeatmap",height="800px"))))
    ,
  tabPanel("PCA / MDS",
    tags$h6("PCA or MDS on the 200 most variable bacteria."),
        sidebarLayout(
          sidebarPanel(
            radioButtons("pcaSamples","Samples to use:",c("Both"="Both","Cases"="Case","Controls"="Control")),
            radioButtons("pcaOrMds","PCA or MDS:",c("PCA"="TRUE","MDS"="FALSE"),selected="FALSE"),
            radioButtons("useDist","Count distances:",c("False"="FALSE","True"="TRUE"),selected="TRUE"),
              conditionalPanel(condition = "input.useDist == 'TRUE'",
                selectInput("distance", "Distance:", 
                    choices=c("euclidean","manhattan","canberra","bray",
                      "kulczynski","jaccard","gower","altGower","morisita",
                      "horn","raup","binomial","chao","cao"),selected="raup")
              ),
            radioButtons("pcaColor","Colored by:",c(
                        "Country" = "Country",
                        "Age" = "AgeFactor",
                        "Health" = "Type",
                        "Dysentery"="Dysentery"),
                        selected = c("Type")),
            numericInput('dimensionx', 'X-axis dimension:', 1,
                 min = 1, max = 4),
            numericInput('dimensiony', 'Y-axis dimension:', 2,
                 min = 1, max = 4),
            br(),
            tags$small("Warning: takes a few seconds")
          ),
          mainPanel(
            plotOutput("pcaPlot",height="650px")
          )
      )
    ),
    tabPanel("Diversity",
      tags$h6("Boxplots of Shannon diversity indexes. Both cases and controls exhibited higher mean Shannon diversity index scores at higher age groups compared to lower age groups. The diversity of healthy samples is higher than diseased samples"),
      sidebarLayout(sidebarPanel(
        checkboxGroupInput("comp",
            "Comparisons:",c("Country"="Country","Age (months)" = "AgeFactor","Health"="Type","Dysentery"="Dysentery"),selected=c("Type"))
        ),
      mainPanel(plotOutput("diversity",height="500px"),tableOutput("diversityTable"))))
    ,
    tabPanel("Rarefaction",
      tags$h6("The linear effect depth of coverage has on the number of bacteria detected. Including each of the three factors in a linear model, the adjusted R^2 is 0.912"),
      sidebarLayout(sidebarPanel(
        checkboxGroupInput("rare",
            "Comparisons:",c("Country"="Country","Age" = "AgeFactor","Health"="Type","Dysentery"="Dysentery"),selected=c("Type"))
        ),
      mainPanel(plotOutput("plotRare",height="600px",width="600px")))),
    tabPanel("OTU Descriptions",
            mainPanel(dataTableOutput("otulist"))
          )
	)
)
