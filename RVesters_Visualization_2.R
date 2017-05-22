#**************************************************************************************************************************************************
# 
#                                            TEAM RVESTERS - VISUALIZATION - PART 2
#
#**************************************************************************************************************************************************
# Note: If the output is not obtained while running the code all at once, kindly perform a manual sequential execution (using ctrl+enter) to obtain a very attractive user interface
#*******************************************************************************************************************************************************************************************


#install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(stringi) 
library(stringr)
library(wordcloud)

#This file is obatined from Visualization_1.R
attach(Top_Freq)
Top_Freq<- read.csv("Top_Freq.csv")

#Removing all the rows with "na"
for (i in 1:nrow(Top_Freq))
{
  if (Top_Freq$Aspect[i]=="na")
  {
    Top_Freq<-Top_Freq[- c(i),]
    
  }
}

#Assigning 'Top_Freq' dataframe values to 'fp'
fp<-Top_Freq

#Column Names are changed
colnames(fp)<-c("Merged","Aspects","Adjective","AspAdj","Sentiment","Frequency","ProductService")

#Forming the dataset for bar-graph representation - Aggregating the data to find out the frequency at an Aspect-Sentiment level
fp_rollup = aggregate(cbind(fp$Frequency) ~ fp$Aspects + fp$Sentiment, data = fp, sum,margin = 1)
fp_rollup_final = reshape(fp_rollup, idvar="fp$Aspects", timevar="fp$Sentiment", direction="wide")
fp_rollup_final = data.frame(fp_rollup_final)
fp_rollup_final[is.na(fp_rollup_final)]=0

#Calculating 'percentage' columns: 'pp'-positive percentage, 'nup'-neutral percentage and 'nep'-Negative percentage of words at an Aspect level
for (i in 1 : nrow(fp_rollup_final))
{
  fp_rollup_final$total[i] = sum(fp_rollup_final$V1.Positive[i],fp_rollup_final$V1.Neutral[i],fp_rollup_final$V1.Negative[i])
  fp_rollup_final$pp[i]=(fp_rollup_final$V1.Positive[i]/fp_rollup_final$total[i])*100
  fp_rollup_final$nup[i]=(fp_rollup_final$V1.Neutral[i]/fp_rollup_final$total[i])*100
  fp_rollup_final$nep[i]=(fp_rollup_final$V1.Negative[i]/fp_rollup_final$total[i])*100
}

#Assigning the data of fp_rollup_final to 'fprf'(Abbreviation)
fprf<-fp_rollup_final

#These are the list of nouns related to products identified from the customer reviews extracted. If these nouns matches with the words in 'Aspect' column, then those rows are tagged as '1'
product_nouns=c("screen",	"android",	"data",	"featuress",	"Money",	"apps",	"network",	"contract",	"battery",	"carrier",	"activation",	"quality",	"camera",	"brand",	"life",	"cell",	"internet",	"website",	"wifi",	"size",	"speaker",	"memory",	"easy",	"upgrade",	"signal",	"coverage",	"model",	"cdma",	"storage",	"fingerprint",	"touch",	"button",	"charger",	"device",	"speed",	"support",	"version",	"sound",	"space",	"port",	"provider",	"unlock",	"charge",	"contract",	"cover",	"email",	"music",	"performance",	"security",	"technology",	"voice",	"facetime",	"itunes",	"software",	"video",	"volume",	"exchange",	"settings",	"texting",	"unlocking",	"videos",	"budget",	"cord",	"games",	"keyboard",	"protector",	"tracking",	"capabilities",	"feature",	"feel",	"hotspot",	"resolution",	"siri",	"scanner",	"standby",	"thumbprint",	"usage",	"voicemail",	"adapter",	"audio",	"backup",	"browser",	"cdma/version",	"downloading",	"durability",	"e-mail",	"facebook",	"functionality",	"feature",	"headphone",	"interface",	"ios",	"jack",	"power",	"processor",	"specs",	"usb",	"weight",	"wi-fi",	"width",	"windows",	"wireless",	"antivirus",
                "sim","picture",	"product",	"iphone",	"phone",	"ipad",	"smartphone",	"i-phone",	"cellphone",	"gadget",	"iphone5",	"mac",	"macbook",	"telephone")

#Comparing the Product based and Service based nouns in Top_freq and 'product_nouns' list
for (i in 1:nrow(Top_Freq))
{ 
  Top_Freq$ProductService[i]=0#Including a new column called as 'ProductService' in order to distinguish the Product related aspects from the Service'
  sep_nouns = unlist(strsplit(as.character(Top_Freq$Aspect[i])," "))#Forming a list of aspects present in 'Top_Freq' table in order to compare them with 'product_nouns'
  for (j in 1: length(sep_nouns))
  {
    if (!is.na(match(sep_nouns[j],product_nouns)))
    {
      Top_Freq$ProductService[i]=1#Product related aspects = 1; Service related aspects = 0
    }
    else
    {
      next 
    }
  }
}

#Including ProductService tag in fprf as well. The same procedure used in Top_Freq for adding 'ProductService' column is followed here.

for (i in 1:nrow(fprf))
{ 
  fprf$ProductService[i]=0
  sep_nouns = unlist(strsplit(as.character(fprf$fp.Aspects[i])," "))
  for (j in 1: length(sep_nouns))
  {
    if (!is.na(match(sep_nouns[j],product_nouns)))
    {
      fprf$ProductService[i]=1
    }
    else
    {
      next 
    }
  }
}

#Subsetting Product-Service from fprf in order to be used in the Positive, Neutral and Negative word clouds
fprf_p <- subset(fprf, fprf$ProductService == 1)
fprf_s <- subset(fprf, fprf$ProductService == 0)

#Product - Postive, Negative, Neutral
fprf_p_pos<-data.frame(fprf_p$fp.Aspects,fprf_p$V1.Positive,fprf_p$ProductService)
fprf_p_neu<-data.frame(fprf_p$fp.Aspects,fprf_p$V1.Neutral,fprf_p$ProductService)
fprf_p_neg<-data.frame(fprf_p$fp.Aspects,fprf_p$V1.Negative,fprf_p$ProductService)

#Service - Postive, Negative, Neutral
fprf_s_pos<-data.frame(fprf_s$fp.Aspects,fprf_s$V1.Positive,fprf_s$ProductService)
fprf_s_neu<-data.frame(fprf_s$fp.Aspects,fprf_s$V1.Neutral,fprf_s$ProductService)
fprf_s_neg<-data.frame(fprf_s$fp.Aspects,fprf_s$V1.Negative,fprf_s$ProductService)

#Used for overall wordcloud - Product/Service
fp_p <- subset(fp, fp$ProductService == 1)
fp_s <- subset(fp, fp$ProductService == 0)

#Forming subsets of Product related rows as Positive, Negative and Neutral
fp_p_pos <- subset(fp_p, fp_p$Sentiment == "Positive")
fp_p_neg <- subset(fp_p, fp_p$Sentiment == "Negative")
fp_p_neu <- subset(fp_p, fp_p$Sentiment == "Neutral")

#Forming subsets of Service related rows as Positive, Negative and Neutral
fp_s_pos <- subset(fp_s, fp_s$Sentiment == "Positive")
fp_s_neg <- subset(fp_s, fp_s$Sentiment == "Negative")
fp_s_neu <- subset(fp_s, fp_s$Sentiment == "Neutral")

#Getting the unique list of nouns for populating in the dropdown by sorting them first
fp_p<-fp_p[order(- fp_p$Frequency),]
fp_s<-fp_s[order(- fp_s$Frequency),]
product_dropdown<-unique(fp_p$Aspects)
service_dropdown<-unique(fp_s$Aspects)

#Data frame 'which 'x' is used for error handling inside the function ShinyApp
x = data.frame(matrix(nrow=1,ncol=2))
colnames(x) <- c("Text","F")
x$Text="No Comments"
x$F = 20


#RShiny User Interface Code
shinyApp(
  # ui - For displaying icons on the User Interface
  ui = fluidPage(theme = shinytheme("flatly"),
                 mainPanel(
                   tabsetPanel(
                     #Product Pane
                     
                     tabPanel("Product",column(12,offset =1,
                              h1("Aspect Based Sentiment Analysis of Walmart Customer Reviews",align = "middle"),
                              #SplitLayout is used for making the graphs share its space in the same row
                              splitLayout(h3("Positive",align="middle"),h3("Neutral",align="middle"),h3("Negative",align="middle")),
                              splitLayout(plotOutput("plot_P_pos"),plotOutput("plot_P_Neu"),plotOutput("plot_P_Neg")),
                              selectInput("Product_Nouns", "Select the product related nouns",
                                          product_dropdown),
                              splitLayout(h3("Positive",align="middle"),h3("Neutral",align="middle"),h3("Negative",align="middle"),h3("Sentiment Analysis",align="middle")),
                              splitLayout(plotOutput("plot_P_pos_dd"),plotOutput("plot_P_neu_dd"),plotOutput("plot_P_neg_dd"),plotOutput("P_barplot"))
                     )),
                     #Service Pane
                     tabPanel("Service",column(12,offset =1,
                              h1("Aspect Based Sentiment Analysis of Walmart Customer Reviews",align="middle"),
                              splitLayout(h3("Positive",align="middle"),h3("Neutral",align="middle"),h3("Negative",align="middle")),
                              splitLayout(plotOutput("plot_S_pos"),plotOutput("plot_S_Neu"),plotOutput("plot_S_Neg")),
                              selectInput("Service_Nouns", "Select the service related nouns",
                                          service_dropdown),
                              splitLayout(h3("Positive",align="middle"),h3("Neutral",align="middle"),h3("Negative",align="middle"),h3("Sentiment Analysis",align="middle")),
                              splitLayout(plotOutput("plot_S_pos_dd"),plotOutput("plot_S_neu_dd"),plotOutput("plot_S_neg_dd"),plotOutput("S_barplot"))))
                   
                   
                 ))),
  #Functions to be performed in the user interface are mentioned in the 'server' function
  server = function(input, output, session) {

    output$plot_P_pos <- renderPlot(
      #Product based positive wordcloud
      wordcloud(words = fprf_p_pos$fprf_p.fp.Aspects, freq = fprf_p_pos$fprf_p.V1.Positive, min.freq = 1, scale = c(15,1.5),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Dark2"))
    )
    
    output$plot_P_Neu <- renderPlot({
      #Product based neutral wordcloud
      wordcloud(words = fprf_p_neu$fprf_p.fp.Aspects, freq = fprf_p_neu$fprf_p.V1.Neutral, min.freq = 1, scale = c(15,1.5),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Paired"))
    })
    output$plot_P_Neg <- renderPlot({
      #Product based negative wordcloud
      wordcloud(words = fprf_p_neg$fprf_p.fp.Aspects, freq = fprf_p_neg$fprf_p.V1.Negative, min.freq = 1, scale = c(15,1.5),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"RdGy"))
    })
    output$plot_S_pos <- renderPlot(
      #Service based positive wordcloud
      wordcloud(words = fprf_s_pos$fprf_s.fp.Aspects, freq = fprf_s_pos$fprf_s.V1.Positive, min.freq = 1, scale = c(8,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Dark2"))
    )
    output$plot_S_Neu <- renderPlot({
      #Service based neutral wordcloud
      wordcloud(words = fprf_s_neu$fprf_s.fp.Aspects, freq = fprf_s_neu$fprf_s.V1.Neutral, min.freq = 1, scale = c(8,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"YlGnBu"))
    })
    output$plot_S_Neg <- renderPlot({
      #Service based negative wordcloud
      wordcloud(words = fprf_s_neg$fprf_s.fp.Aspects, freq = fprf_s_neg$fprf_s.V1.Negative, min.freq = 1, scale = c(8,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"YlOrRd"))
    })
    
    output$plot_P_pos_dd <- renderPlot({
      #Plotting the positive word cloud of adjectives
      pn<-input$Product_Nouns
      p = subset(fp_p,fp_p$Aspects==pn)
      pp=subset(p,p$Sentiment=="Positive")
      if (nrow(pp)==0)# If pp is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen 
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(2,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"Dark2"))
      }
      else
      {
      wordcloud(words = pp$Adjective, freq = pp$Frequency, scale = c(7,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"PiYG"))
      }
    })
    output$plot_P_neu_dd <- renderPlot({
      #Plotting the Product based Neutral word cloud of adjectives
      pn<-input$Product_Nouns
      p = subset(fp_p,fp_p$Aspects==pn)
      pNeu=subset(p,p$Sentiment=="Neutral")
      if (nrow(pNeu)==0)# If pNeu is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(2,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"YlGnBu"))
      }
      else
      {
      wordcloud(words = pNeu$Adjective, freq = pNeu$Frequency, scale = c(7,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Paired"))
      }
      
    })
    output$plot_P_neg_dd <- renderPlot({
      #Plotting the Product based Negative word cloud of adjectives
      pn<-input$Product_Nouns
      p = subset(fp_p,fp_p$Aspects==pn)
      pNeg=subset(p,p$Sentiment=="Negative")
      if (nrow(pNeg)==0)# If pNeg is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(2,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"RdGy"))
      }
      else
      {
        wordcloud(words = pNeg$Adjective, freq = pNeg$Frequency, scale = c(7,0.7),
                  max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                  colors=brewer.pal(9,"RdGy"))
      }
    })
    output$plot_S_pos_dd <- renderPlot({
      #Plotting the Service based Positive word cloud of adjectives
      sn<-input$Service_Nouns
      s = subset(fp_s,fp_s$Aspects==sn)
      sp=subset(s,s$Sentiment=="Positive")
      if (nrow(sp)==0)# If sp is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(2,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"Greens"))
      }
      else
      {
      wordcloud(words = sp$Adjective, freq = sp$Frequency, scale = c(5,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Greens"))
      }
      
    })
    output$plot_S_neu_dd <- renderPlot({
      #Plotting the Service based Neutral word cloud of adjectives
      sn<-input$Service_Nouns
      s = subset(fp_s,fp_s$Aspects==sn) 
      sNeu=subset(s,s$Sentiment=="Neutral")
      if (nrow(sNeu)==0)# If sNeu is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(2,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"Blues"))
      }
      else
      {
      wordcloud(words = sNeu$Adjective, freq = sNeu$Frequency, scale = c(5,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Blues"))
      }
    })
    output$plot_S_neg_dd <- renderPlot({
      #Plotting the Service based Negative word cloud of adjectives
      sn<-input$Service_Nouns
      s = subset(fp_s,fp_s$Aspects==sn) 
      sNeg=subset(s,s$Sentiment=="Negative")
      if (nrow(sNeg)==0)# If sNeg is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(2,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"PuRd"))
      }
      else
      {
      wordcloud(words = sNeg$Adjective, freq = sNeg$Frequency, scale = c(5,0.7),
                max.words=200,random.order=FALSE, rot.per=0.35, fixed.asp = T,
                colors=brewer.pal(9,"Reds"))
      }
    })
    output$P_barplot <- renderPlot({
      #Bar Plot for Product Nouns
      pn<-input$Product_Nouns
      rx <- subset(fprf,fprf$fp.Aspects==pn)
      if (nrow(rx)==0)# If 'rx' is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(5,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"PuRd"))
      }
      else
      {#Plotting the percentage of the postive, neutral and the negative comments associated with the product aspects
        barplot(c(rx$pp,rx$nup,rx$nep),names.arg =  c('Positive','Neutral','Negative'), col = c('Green', 'Blue', 'Red'))        
      }
    })
    output$S_barplot <- renderPlot({
      #Bar Plot for Service Nouns
      sn<-input$Service_Nouns
      sx <- subset(fprf,fprf$fp.Aspects==sn)
      if (nrow(sx)==0)# If 'sx' is an empty dataframe, "No Comments" present in the dataframe "x" will be displayed on the screen
      {
        wordcloud(words = x$Text, freq = x$F, scale = c(5,0.7),
                  max.words=200,random.order=FALSE, fixed.asp = T,
                  colors=brewer.pal(9,"PuRd"))
      }
      else
      { #Plotting the percentage of the postive, neutral and the negative comments associated with the service aspects
        barplot(c(sx$pp,sx$nup,sx$nep),names.arg =  c('Positive','Neutral','Negative'), col = c('Green', 'Blue', 'Red')) 
      }
    })
  }#Server bracket ends here
)





