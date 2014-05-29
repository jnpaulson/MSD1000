download_not_installed<-function(x){    
    for(i in x){
        if(!require(i,character.only=TRUE)){
         install.packages(i,repos="http://cran.r-project.org")
         library(i,character.only=TRUE)
        }       
    }   
}
required_packages = c("shiny")
download_not_installed(required_packages)

if(!require("metagenomeSeq")){
        source("http://bioconductor.org/biocLite.R")
        biocLite("metagenomeSeq")
        library("metagenomeSeq")
}

load("forserveroptim.rdata")
     shinyUI(navbarPage("MSD 1000",
      tabPanel("Feature Abundance plots",
        sidebarLayout(
          sidebarPanel(
            radioButtons("level","Level",c(
                        "Phylum" = "phylum",
                        "Class" = "class",
                        "Genus" = "genus","Species"="species","OTU"="OTU"),
                        selected = c("OTU")),
              conditionalPanel(condition = "input.level == 'OTU'",
                numericInput('feature', 'OTU:', 1, min = 1, max = 197358),
                checkboxInput("otu","Index",TRUE)
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
            br(),
            checkboxInput("norm", "normalized data", TRUE),
            br(),
            br(),
            checkboxGroupInput("country","Include country:",c(
                        "Kenya" = "Kenya",
                        "The Gambia" = "Gambia",
                        "Mali" = "Mali","Bangladesh"="Bangladesh"),
                        selected = c("Kenya","Gambia","Mali","Bangladesh")),
            checkboxGroupInput("age","Include age:",c(
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
            br()
          ),
          mainPanel(
            plotOutput("plot"),
            conditionalPanel(condition = "input.level == 'OTU'",
#            tableOutput("summary"),
              tableOutput("table")
            )
          )
      )
    ),
    tabPanel("Diversity",
      sidebarLayout(sidebarPanel(
        checkboxGroupInput("comp",
            "Comparisons",c("Country"="Country","Age" = "AgeFactor","Health"="Type","Dysentery"="Dysentery"),selected=c("Type"))
        ),
      mainPanel(plotOutput("diversity"),tableOutput("diversityTable"))))
    ,
    navbarMenu("More",
      tabPanel("OTU descriptions",
            mainPanel(
                dataTableOutput("otulist")
             )
          )
         )
       )
)
