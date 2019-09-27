

############################### Popular Baby Names #####################################

########################################################################################



if(!require("pacman"))install.packages("pacman")
pacman::p_load(shiny, tidyverse, readxl, dplyr, shinythemes)



# Reading Data
df_girls <- read_excel("Top100_Popular_Baby_Names.xlsx")
#View(df_girls)


##################################### DATA WRANGLING ##########################################


################  GIRLS   ###################

# Deleting last 3 empty rows of the dataset
n<-dim(df_girls)[1]
df_girls<-df_girls[1:(n-3),]

# Deleting the first 3 rows of the data
df_girls <- df_girls[4:nrow(df_girls), ]
df_girls1 <- df_girls
#View(df_girls1)

# Editing the First row of the dataset
#df_girls1[1, ] <- zoo::na.locf(unlist(df_girls1[1,]))
df_girls1[1, ] <- zoo::na.locf(unlist(df_girls1[1,]))
#View(df_girls1)



# Cleaning the dataset of NA columns, rows and adding proper header
df_girls1 <- df_girls1[,-1] 
df_girls1 <- df_girls1[-3,] 

df_girls2 <- df_girls1
toDelete <- seq(1, ncol(df_girls2), 3)
df_girls2 <- df_girls2[,-toDelete]
#View(df_girls2)

df_girls3 <- df_girls2[-2,] 

#View(df_girls3)

j=ncol(df_girls3)

cols <- c()

for (i in 1:j)
{
  if (!i %% 2)
    {
    cols[i] <- as.character(paste(df_girls3[1,i],"Counts",sep="_"))
  }
  else{
  cols[i] <- as.character(paste(df_girls3[1,i],"Names",sep="_"))
  }
}
cols
colnames(df_girls3) <- cols
df_girls3 <- df_girls3[-1,] 

#View(df_girls3)


################  BOYS   ###################

df_boys <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet=2)
#View(df_boys)

# Deleting last 3 empty rows of the dataset
n<-dim(df_boys)[1]
df_boys<-df_boys[1:(n-3),]

# Deleting the first 3 rows of the data
df_boys <- df_boys[4:nrow(df_boys), ]
df_boys1 <- df_boys
#View(df_boys1)

# Editing the First row of the dataset
df_boys1[1, ] <- zoo::na.locf(unlist(df_boys1[1,]))
#View(df_boys1)



# Cleaning the dataset of NA columns, rows and adding proper header
df_boys1 <- df_boys1[,-1] 
df_boys1 <- df_boys1[-3,] 

df_boys2 <- df_boys1
toDelete <- seq(1, ncol(df_boys2), 3)
df_boys2 <- df_boys2[,-toDelete]
#View(df_boys2)

df_boys3 <- df_boys2[-2,] 

#View(df_boys3)

j=ncol(df_boys3)

cols <- c()

for (i in 1:j)
{
  if (!i %% 2)
  {
    cols[i] <- as.character(paste(df_boys3[1,i],"Counts",sep="_"))
  }
  else{
    cols[i] <- as.character(paste(df_boys3[1,i],"Names",sep="_"))
  }
}
cols
colnames(df_boys3) <- cols

df_boys3 <- df_boys3[-1,]

#View(df_boys3)

Girls_list <- df_girls3[,seq(1,129,2)]%>%gather(value="girls")%>%select("girls")%>%unique()%>%arrange(girls)

Boys_list <- df_boys3[,seq(1,129,2)]%>%gather(value="boys")%>%select("boys")%>%unique()%>%arrange(boys)


#View(Girls_list)
#View(Girls_list)
#class(Boys_list)

#Boys_list




###########################################  SHINY APP  ##########################################




ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("Popularity of Baby Names"),
  sidebarLayout(
    sidebarPanel(
     
      selectInput(inputId="year", label="Select the Year", choices=seq(1954,2018), selected="1954"),
      selectInput(inputId ="Girl_name", label="Select the Girl name", choices = Girls_list, selected = "Christian"),
      selectInput(inputId="Boy_name", label="Select the Boy name", choices = Boys_list, selected = "John")
	  
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Popular Names of Girls by Year", span(style = "font-weight:bold;", tableOutput("table_girls"))),
        tabPanel("Popular Names of Boys by Year", span(style = "font-weight:bold;", tableOutput("table_boys"))), 
        tabPanel("Time Analysis of a Girl's Name", plotOutput("plot_girls")),
		    tabPanel("Time Analysis of a Boy's Name", plotOutput("plot_boys"))
      )
    )
  )
)


server <- function(input, output) 
  {
  
  output$table_girls <- renderTable({
    
    column <- c(paste(input$year,"Names",sep="_"),paste(input$year,"Counts",sep="_"))
    selected_cols <-  df_girls3[,column]
    tb_girls <- selected_cols[order(paste(input$year,"Counts",sep="_"))]
    head(tb_girls,10)
  })
  
  output$table_boys <- renderTable({
    
    column <- c(paste(input$year,"Names",sep="_"),paste(input$year,"Counts",sep="_"))
    selected_cols <-  df_boys3[,column]
    tb_boys <- selected_cols[order(paste(input$year,"Counts",sep="_"))]
    head(tb_boys,10)
  })
  
  output$plot_girls <- renderPlot({
    
    input <- input$Girl_name
    rank_girl <- data.frame(year = seq(1954,2018), ranking = 0)
    for (i in seq(1954,2018)){
      temp=unlist(df_girls3[,as.character(paste(i,"Names",sep="_"))])
      rank_girl[rank_girl$year==i,'ranking'] <-if(input %in% temp)which(temp==input)else 0
      if(rank_girl[rank_girl$year==i,'ranking']!=0)
      {
        rank_girl[rank_girl$year==i,'ranking']<-101-rank_girl[rank_girl$year==i,'ranking']
      }
        
    }
    plot(xlab="Year",ylab="Popularity",type="h",rank_girl$year, rank_girl$ranking)
   
    
    
    
  })
  
  output$plot_boys <- renderPlot({
    
    input <- input$Boy_name
    rank_boy <- data.frame(year = seq(1954,2018), ranking = 0)
    for (i in seq(1954,2018)){
      temp=unlist(df_boys3[,as.character(paste(i,"Names",sep="_"))])
      rank_boy[rank_boy$year==i,'ranking'] <-if(input %in% temp)which(temp==input)else 0
      if(rank_boy[rank_boy$year==i,'ranking']!=0)
      {
        rank_boy[rank_boy$year==i,'ranking']<-101-rank_boy[rank_boy$year==i,'ranking']
      }
      
    }
    plot(xlab="Year",ylab="Popularity",type="h",rank_boy$year, rank_boy$ranking)
    
    
    
    
  })
  
}




shinyApp(ui, server)
  
  
  
################################  END  ###########################################

