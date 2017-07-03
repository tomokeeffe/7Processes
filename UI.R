library(shinydashboard)
library(rsconnect)
library(shiny)
library(plotly)
library(reshape2)
library(dplyr)
library(zoo)
library(ecb)
library(lubridate)

load(file="AllData.RData")
load(file="M3.RData")


#UI
sidebar <- dashboardSidebar(
  sidebarMenu(id="menu1",
              menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
              menuItem("Figures", icon = icon("th"), tabName = "Figures",badgeColor = "green"),
              menuItem("Country Analysis",  icon = icon("th"), tabName = "Country",badgeColor = "green")
  ),
  conditionalPanel(condition="input.menu1=='Figures'",
                   selectInput(inputId = "geo.time",
                               label = "Select Country",
                               choices = sort(as.character(unique(DF$geo.time))),
                               selected = "Ireland")
  ),
  conditionalPanel(condition="input.menu1=='Figures' && input.figure == 2 || input.figure == 4|| input.figure == 6",
                   selectInput(inputId = "geo2",
                               label = "Select Country 2",
                               choices = sort(as.character(unique(DF$geo.time))),
                               selected = "UK")
  ),
  conditionalPanel(condition="input.menu1=='Figures'",
                   selectInput("figure", "Selected Graphs", 
                               choices = list("General Government Balance"=1, "Fiscal Ratio and GDP"=2, "Current Balance of Payments"=3,"Trade Ratio and GDP"=4,"Combined Fiscal-Trade Ratio"=5,"Combined Fiscal-Trade Ratio and GDP"=6,"Sectoral Balances"=7,"Real Private Exp. and Disposable Income"=8,"Private Financial Deficit"=9,"Private Balance and Net Lending"=10,"Growth in M3"=11 ),
                               selected = 5)
  ),
  conditionalPanel(condition="input.menu1=='Country'",
                   selectInput(inputId = "country",
                               label = "Select Country",
                               choices = sort(as.character(unique(DF$geo.time))),
                               selected = "Ireland")
  ))

body <- dashboardBody(
  fluidPage(
    tabItems(
      tabItem(tabName = "Introduction",
              fluidRow(box(title = "Introduction",width = "100",status= "success",  solidHeader = TRUE, collapsible = TRUE,
                           "Wynne Godley's paper",span(a("Seven Unsustainable Processes (1999)",href="http://www.google.ie/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0CCAQFjAAahUKEwjnqpyepv7HAhUEuBoKHQb4DHQ&url=http%3A%2F%2Fwww.levyinstitute.org%2Fpubs%2Fsevenproc.pdf&usg=AFQjCNEF7Vf6q7BRf7G0QSJim1cxeRhsjQ&sig2=ABgwGHEbqTgi2CxWx2b4uQ",target="_blank")), "which examines the medium-term prospects of the US economy and possible future policies, shows that in the United States, growth in that period was associated with seven unsustainable processes.  Godley argues that, given unchanged fiscal policy and growth in the rest of the world, in order to maintain growth, the excessive indebtedness implied by these processes would be so large as to create major problems for the US economy in the future.
                           This web application aims to replicate the graphs in Godley's 1999 paper for all countries in the EU, to see whether or not similar patterns emerge in other countries."      
              )),
              fluidRow(box(title = "How to Use this Web Application",width = "100",status= "success",  solidHeader = TRUE, collapsible = TRUE,
                           "Click the Figures tab in the sidebar and click the relevant button to show the plotly graph of your choice. Select the Country from the drop down box in the sidebar to generate the data for that country.All the charts are interactive, click and drag to zoom in and double-click to return to the original graph. Drag the corners of a graph to zoom along one axis. Double-click to autoscale a single axis. By clicking on the legend items, these items can be removed and added."
              )),
              
              fluidRow(box(title = "Notes on Data",width = "100",status= "success",  solidHeader = TRUE, collapsible = TRUE,
                           "This web application utilises R to collect data from the", span(a("European Sector Accounts",href="http://ec.europa.eu/eurostat/web/sector-accounts",target="_blank")), "the European equivalent of the National Income and Product Accounts (NIPA) Godley makes use of in his paper. Data on the money stock is collected from the", span(a("ECB's Statistical Warehouse",href="http://sdw.ecb.europa.eu/",target="_blank")),". Note that for some countries data may be incomplete. An ARIMA(p,d,q) model is generated to forecast the next 5 years of data including the 95% confidence interval, where p is the Autoregressive order, d is the order of differencing and q is the Moving Average order."
              ))
              
              ),    
      
      tabItem(tabName = "Figures",
              fluidRow(box(title=textOutput("boxtitle"),status = "warning",width = "100%",solidHeader = T,textOutput("text"), collapsible = TRUE)),
              fluidRow(box(plotlyOutput(("plot")), width = "100%",status = "success", solidHeader = TRUE, collapsible = TRUE, title= "Chart 1")),
              conditionalPanel(condition = "input.figure == 2 || input.figure == 4 || input.figure == 5 || input.figure == 6 || input.figure == 8",fluidRow(box(plotlyOutput(("plot2")), width = "100%",status = "success", solidHeader = TRUE, collapsible = TRUE, title= "Chart 2"))),
              conditionalPanel(condition = "input.figure == 2 || input.figure == 4|| input.figure == 6",fluidRow(box(plotlyOutput(("plot3")), width = "100%",status = "success", solidHeader = TRUE, collapsible = TRUE, title= "Chart 3"))),
              
              tags$head(tags$style(HTML('
                                        
                                        /* logo */
                                        .skin-blue .main-header .logo {
                                        background-color: #00a65a;
                                        font-family: "Georgia", Times, "Times New Roman", serif;
                                        font-weight: bold;
                                        font-size: 24px;
                                        }
                                        
                                        /* logo when hovered */
                                        .skin-blue .main-header .logo:hover {
                                        background-color: #00a65a;
                                        }
                                        
                                        /* navbar (rest of the header) */
                                        .skin-blue .main-header .navbar {
                                        background-color: #00a65a;
                                        }
                                        /* main sidebar */
                                        .skin-blue .main-sidebar {
                                        background-color: #fff0;
                                        }
                                        
                                        .content-wrapper,
                                        .right-side {
                                        background-color: #ffffff;
                                        font-size: 15px;
                                        line-height: 1.7;
                                        }
                                        
                                        .box.box-solid.box-success>.box-header {
                                        height: 30px;
                                        line-height:0;
                                        }
                                        .box-header .box-title{
                                        font-size: 16px;
                                        font-weight: bold;
                                        
                                        }
                                        /* main sidebar */
                                        .skin-blue .main-sidebar {
                                        background-color: #404040;
                                        }
                                        
                                        
                                        /* active selected tab in the sidebarmenu */
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                        background-color: #303030;
                                        color: #ffff;
                                        border-left-color: #00a65a;
                                        }
                                        
                                        /* other links in the sidebarmenu */
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                        background-color: #404040;
                                        color: #ffff;
                                        }
                                        
                                        /* other links in the sidebarmenu when hovered */
                                        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                        background-color: #303030
                                        color: #ffff;
                                        border-left-color: #00a65a;
                                        }
                                        /* toggle button when hovered  */                    
                                        .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                        background-color: #303030;
                                        }
                                        ')))
              ),    
      
      tabItem(tabName = "Country",
              fluidRow(box(title = "Fiscal Policy",width = 12,status= "warning",  solidHeader = TRUE, collapsible = TRUE,
                           "The Adjusted Fiscal Ratio, Combined Fiscal-Trade Ratio and General Government Balance are used to demonstrate each government's fiscal stance. Through these graphs, the trend in each countries government inflows, outflows and debts can be seen easily. The AFR will be equal to GDP when the budget is balanced, exceed GDP when the budget is in deficit and fall below GDP when in surplus.")),
              fluidRow(box(plotlyOutput("CFTRplot"),width = 12, status = "success",solidHeader = TRUE, collapsible = TRUE, title= " ")),
              fluidRow(box(plotlyOutput("AFRplot"),width = 6,status = "success", solidHeader = TRUE, collapsible = TRUE, title= " "),
                       box(plotlyOutput("GenGov"),width = 6,status = "success", solidHeader = TRUE, collapsible = TRUE, title= " ")),
              
              fluidRow(box(title = "Foreign Trade and Payments",width = 12,status= "warning",  solidHeader = TRUE, collapsible = TRUE,
                           "Each country's foreign trade trends can be seen in the following charts. Through these graphs, the inflows and outflows of money can be seen as each country's Adjusted Trade Ratio, Combined Fiscal Trade Ratio and Balance of Payments are depicted below. The ATR will track GDP if the balance of payments is balanced, exceed GDP if the balance of payments is positive and fall below GDP if the balance of payments is negative. ")),
              fluidRow(box(plotlyOutput("CFTRplot2"),width = 12, status = "success",solidHeader = TRUE, collapsible = TRUE, title= " ")),
              fluidRow(box(plotlyOutput("ATRplot"),width = 6,status = "success", solidHeader = TRUE, collapsible = TRUE, title= " "),
                       box(plotlyOutput("BOPplot"),width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE, title= " ")),
              
              fluidRow(box(title = "Private Saving, Spending and Borrowing",width = 12,status= "warning",  solidHeader = TRUE, collapsible = TRUE,
                           "Each country's spending, saving and borrowing trends are displayed in the figures below. These graphs illustrate the flow of payments and debt Households(HH) and Non-Financial Corporations(NFC) within in each country. ")),
              fluidRow(box(plotlyOutput("DispIncplot"),width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, title= " ")), 
              fluidRow(box(plotlyOutput("SavInv"),width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE, title= " "),
                       box(plotlyOutput("PrivBalplot"),width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE, title= " "))
              
      )
      
              )))






              
shinyUI(dashboardPage(
  dashboardHeader(title = "Unsustainable Processes of the EU",titleWidth = "100%"),
  sidebar,
  body
))



