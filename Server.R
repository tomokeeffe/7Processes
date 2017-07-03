library(shinydashboard)
library(rsconnect)
library(shiny)
library(plotly)
library(reshape2)
library(dplyr)
library(TSA)
library(forecast)

load(file="AllData.RData")
load(file="M3.RData")

shinyServer(function(input, output) {
  
  DF2 <- DF
  
  output$plot<- renderPlotly({
    
    if(input$figure==1){
      DF <- filter(DF, geo.time == input$geo.time)
      
      p1 <- plot_ly(DF, x = DF$time, y = DF$gen_gov_balance_pc, type = "bar")%>%
        layout(title = "Figure 1 - General Government Balance", yaxis = list(title = "Percentage of GDP"), xaxis = list(title = "Time"))
      
    }else
      if(input$figure==2){
        DF <- filter(DF, geo.time == input$geo.time)
        
        GDPts<- subset(DF, select=c("gdp"))
        GDPts <- na.omit(ts(GDPts,start = c(1949, 1),frequency = 1))
        GDPfit<-auto.arima(GDPts)
        GDPfore<-forecast(GDPfit,h=5)
        
        AFRts<- subset(DF, select=c("AFR"))
        AFRts <- na.omit(ts(AFRts,start = c(1949, 1),frequency = 1))
        AFRfit<-auto.arima(AFRts)
        AFRfore<-forecast(AFRfit,h=5)
        
        p2<-plot_ly()%>%
          add_lines(x = time(GDPts), y = GDPts,line=list(width = 3), name = 'GDP') %>%
          add_lines(x = time(AFRts), y = AFRts, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'AFR')%>%
          add_trace(x = time(GDPfore$mean), y = GDPfore$mean, color = I("navy"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "GDP Prediction")%>%
          add_trace(x = time(AFRfore$mean), y = AFRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "AFR Prediction")%>%
          add_ribbons(x = time(GDPfore$mean), ymin = GDPfore$lower[, 2], ymax = GDPfore$upper[, 2],color = I("slategray2"), name = "GDP 95% confidence")%>%
          add_ribbons(x = time(AFRfore$mean), ymin = AFRfore$lower[, 2], ymax = AFRfore$upper[, 2],color = I("wheat1"), name = "AFR 95% confidence")%>%
          layout(title = "Figure 2 - Adjusted Fiscal Ratio and GDP",xaxis = list(title = 'Time'),yaxis = list(title = '€ in Millions'))
        
      } else
        
        if(input$figure==3){
          DF <- filter(DF, geo.time == input$geo.time)
          
          p3 <- plot_ly( x = ~DF$time, y = ~DF$bop_pc, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'Current BOP')%>%
            add_trace(x = ~DF$time, y = DF$bop_goods_pc, type = 'scatter', mode = 'lines',line=list(width = 3),name = 'BOP Goods')%>%
            add_trace(x = ~DF$time, y = ~DF$bop_soods_pc, type = 'scatter', mode = 'lines',line=list(width = 3),name = 'BOP Services')%>%
            add_trace(x = ~DF$time, y = ~DF$pb_bal_pc, line=list(width = 3),name = 'Income Balances')%>%
            layout(title = "Figure 3 - Current Balance of Payments",xaxis = list(title = 'Time'),
                   yaxis = list(title = 'Percentage of GDP'))
          
        }else
          
          if(input$figure==4){
            
            DF <- filter(DF, geo.time == input$geo.time)
            
            GDPts<- subset(DF, select=c("gdp"))
            GDPts <- na.omit(ts(GDPts,start = c(1949, 1),frequency = 1))
            GDPfit<-auto.arima(GDPts)
            GDPfore<-forecast(GDPfit,h=5)
            
            ATRts<- subset(DF, select=c("ATR"))
            ATRts <- na.omit(ts(ATRts,start = c(1949, 1),frequency = 1))
            ATRfit<-auto.arima(ATRts)
            ATRfore<-forecast(ATRfit,h=5)
            
            p4<-plot_ly()%>%
              add_lines(x = time(GDPts), y = GDPts, line=list(width = 3),name = 'GDP') %>%
              add_lines(x = time(ATRts), y = ATRts, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'ATR')%>%
              add_trace(x = time(GDPfore$mean), y = GDPfore$mean, color = I("navy"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "GDP Prediction")%>%
              add_trace(x = time(ATRfore$mean), y = ATRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "ATR Prediction")%>%
              add_ribbons(x = time(GDPfore$mean), ymin = GDPfore$lower[, 2], ymax = GDPfore$upper[, 2],color = I("slategray2"), name = "GDP 95% confidence") %>%
              add_ribbons(x = time(ATRfore$mean), ymin = ATRfore$lower[, 2], ymax = ATRfore$upper[, 2],color = I("wheat1"), name = "AFR 95% confidence") %>%
              layout(title = "Figure 4 - Adjusted Trade Ratio and GDP", xaxis = list(title = 'Time'),
                     yaxis = list(title = '€ in Millions'))            
            
          }else
            
            if(input$figure==5){ 
              DF <- filter(DF, geo.time == input$geo.time)
              
              AFRts<- subset(DF, select=c("AFR"))
              AFRts <- na.omit(ts(AFRts,start = c(1949, 1),frequency = 1))
              AFRfit<-auto.arima(AFRts)
              AFRfore<-forecast(AFRfit,h=5)
              
              ATRts<- subset(DF, select=c("ATR"))
              ATRts <- na.omit(ts(ATRts,start = c(1949, 1),frequency = 1))
              ATRfit<-auto.arima(ATRts)
              ATRfore<-forecast(ATRfit,h=5)
              
              CFTRts<- subset(DF, select=c("CFTR"))
              CFTRts <- na.omit(ts(CFTRts,start = c(1949, 1),frequency = 1))  
              CFTRfit<-auto.arima(CFTRts)
              CFTRfore<-forecast(CFTRfit,h=5)
              
              p5<-plot_ly()%>%
                add_lines(x = time(AFRts), y = AFRts,line=list(width = 3), name = 'AFR') %>%
                add_lines(x = time(ATRts), y = ATRts, type = 'scatter', line=list(width = 3),mode = 'lines', name = 'ATR')%>%
                add_lines(x = time(CFTRts), y = CFTRts, type = 'scatter', line=list(width = 3),mode = 'lines', name = 'CFTR')%>%
                add_trace(x = time(AFRfore$mean), y = AFRfore$mean, color = I("navy"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "AFR Prediction")%>%
                add_trace(x = time(ATRfore$mean), y = ATRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "ATR Prediction")%>%
                add_trace(x = time(CFTRfore$mean), y = CFTRfore$mean, color = I("darkgreen"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "CFTR Prediction")%>%
                #add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],color = I("slategray2"), name = "GDP 95% confidence") %>%
                #add_ribbons(x = time(fore2$mean), ymin = fore2$lower[, 2], ymax = fore2$upper[, 2],color = I("wheat1"), name = "AFR 95% confidence") %>%
                layout(title = "Figure 5 - Combined Fiscal-Trade Ratio",xaxis = list(title = 'Time'),
                       yaxis = list(title = '€ in Millions'))        
              
            }else
              if(input$figure==6){
                
                DF <- filter(DF, geo.time == input$geo.time)
                
                GDPts<- subset(DF, select=c("gdp"))
                GDPts <- na.omit(ts(GDPts,start = c(1949, 1),frequency = 1))
                GDPfit<-auto.arima(GDPts)
                GDPfore<-forecast(GDPfit,h=5)
                
                CFTRts<- subset(DF, select=c("CFTR"))
                CFTRts <- na.omit(ts(CFTRts,start = c(1949, 1),frequency = 1))  
                CFTRfit<-auto.arima(CFTRts)
                CFTRfore<-forecast(CFTRfit,h=5)
                
                p6<-plot_ly()%>%
                  add_lines(x = time(GDPts), y = GDPts, line=list(width = 3),name = 'GDP') %>%
                  add_lines(x = time(CFTRts), y = CFTRts, type = 'scatter', line=list(width = 3),mode = 'lines', name = 'CFTR')%>%
                  add_trace(x = time(GDPfore$mean), y = GDPfore$mean, color = I("navy"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "GDP Prediction")%>%
                  add_trace(x = time(CFTRfore$mean), y = CFTRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "CFTR Prediction")%>%
                  add_ribbons(x = time(GDPfore$mean), ymin = GDPfore$lower[, 2], ymax = GDPfore$upper[, 2],color = I("slategray2"), name = "GDP 95% confidence") %>%
                  add_ribbons(x = time(CFTRfore$mean), ymin = CFTRfore$lower[, 2], ymax = CFTRfore$upper[, 2],color = I("wheat1"), name = "CFTR 95% confidence") %>%
                  layout(title = "Figure 6 - Combined Fiscal-Trade Ratio and GDP",xaxis = list(title = 'Time'),
                         yaxis = list(title = '€ in Millions')) 
                
                
              }else
                
                if(input$figure==7){
                  DF <- filter(DF, geo.time == input$geo.time)
                  
                  Spcts<- subset(DF, select=c("S_pc"))
                  Spcts <- na.omit(ts(Spcts,start = c(1949, 1),frequency = 1))
                  Sfit<-auto.arima(Spcts)
                  Sfore<-forecast(Sfit,h=5)
                  
                  GDEFts<- subset(DF, select=c("GDEF_pc"))
                  GDEFts <- na.omit(ts(GDEFts,start = c(1949, 1),frequency = 1))
                  GDfit<-auto.arima(GDEFts)
                  GDfore<-forecast(GDfit,h=5)
                  
                  CApcts<- subset(DF, select=c("CA_pc"))
                  CApcts <- na.omit(ts(CApcts,start = c(1949, 1),frequency = 1))
                  CAfit<-auto.arima(CApcts)
                  CAfore<-forecast(CAfit,h=5)
                  
                  
                  p7 <- plot_ly( x = time(Spcts), y = Spcts, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'Private Balance') %>%
                    add_trace(x = time(GDEFts), y = GDEFts, line=list(width = 3),name = 'Government Balance')%>%
                    add_trace(x = time(CApcts), y = CApcts, line=list(width = 3),name = 'Rest of World')%>%
                    add_trace(x = time(Sfore$mean), y = Sfore$mean, color = I("navy"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "Priv Bal Prediction")%>%
                    add_trace(x = time(GDfore$mean), y = GDfore$mean, color = I("darkorange2"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "Gov Bal Prediction")%>%
                    add_trace(x = time(CAfore$mean), y = CAfore$mean, color = I("darkgreen"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "ROW Prediction")%>%
                    layout(title = "Figure 7 - The Three major Financial Balances (NFTR)",xaxis = list(title = 'Time'),
                           yaxis = list(title = 'Percentage of GDP'))
                  
                }else
                  
                  if(input$figure==8){
                    
                    DF <- filter(DF, geo.time == input$geo.time)
                    
                    disp_incts<- subset(DF, select=c("disp_inc"))
                    disp_incts <- na.omit(ts(disp_incts,start = c(1949, 1),frequency = 1))
                    DIfit<-auto.arima(disp_incts)
                    DIfore<-forecast(DIfit,h=5)
                    
                    PrvExpTs<- subset(DF, select=c("prv_exp"))
                    PrvExpTs <- na.omit(ts(PrvExpTs,start = c(1949, 1),frequency = 1))
                    PEfit<-auto.arima(PrvExpTs)
                    PEfore<-forecast(PEfit,h=5)
                    
                    
                    
                    p8 <- plot_ly() %>%
                      add_trace(x = ~DF$time, y = ~DF$disp_inc, type = 'scatter', mode = 'lines', name = 'Real Private Exp.')%>%  #fill='tozeroy'#
                      add_trace(x = ~DF$time, y = ~DF$prv_exp, type = 'scatter', mode = 'lines',name = 'Real Disp. Income')%>% #, fill='tozeroy'
                      add_trace(x = time(DIfore$mean), y = DIfore$mean, color = I("navy"), name = "Priv Exp Prediction",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
                      add_trace(x = time(PEfore$mean), y = PEfore$mean, color = I("darkorange2"), name = "Disp Inc Prediction",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
                      add_ribbons(x = time(DIfore$mean), ymin = DIfore$lower[, 2], ymax = DIfore$upper[, 2],color = I("slategray2"), name = "Priv Exp 95% confidence") %>%
                      add_ribbons(x = time(PEfore$mean), ymin = PEfore$lower[, 2], ymax = PEfore$upper[, 2],color = I("wheat1"), name = "Disp Inc 95% confidence") %>%
                      layout(title = "Figure 8 - Real Private Expenditure and Disposable Income", xaxis = list(title = 'Time'),
                             yaxis = list(title = '€ in Millions'))
                    
                  }else
                    
                    if(input$figure==9){
                      DF <- filter(DF, geo.time == input$geo.time)
                      
                      NFCts<- subset(DF, select=c("nfc_sav_m_inv_pc"))
                      NFCts <- na.omit(ts(NFCts,start = c(1949, 1),frequency = 1))
                      NFCfit<-auto.arima(NFCts)
                      NFCfore<-forecast(NFCfit,h=5)
                      
                      HHts<- subset(DF, select=c("hh_sav_m_inv_pc"))
                      HHts <- na.omit(ts(HHts,start = c(1949, 1),frequency = 1))
                      HHfit<-auto.arima(HHts)
                      HHfore<-forecast(HHfit,h=5)
                      
                      p9 <- plot_ly() %>%
                        add_trace( x = ~DF$time, y = ~DF$nfc_sav_m_inv_pc, type = 'scatter', mode = 'lines', name = "NFC SAV-INV") %>%
                        add_trace(x = ~DF$time, y = ~DF$hh_sav_m_inv_pc,type = 'scatter', mode = 'lines', name = 'HH SAV-INV')%>%
                        add_trace(x = time(NFCfore$mean), y = NFCfore$mean, color = I("navy"), name = "NFC SAV-INV Prediction",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
                        add_trace(x = time(HHfore$mean), y = HHfore$mean, color = I("darkorange2"), name = "HH SAV-INV Prediction",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
                        add_ribbons(x = time(NFCfore$mean), ymin = NFCfore$lower[, 2], ymax = NFCfore$upper[, 2],color = I("slategray2"), name = "NFC 95% confidence") %>%
                        add_ribbons(x = time(HHfore$mean), ymin = HHfore$lower[, 2], ymax = HHfore$upper[, 2],color = I("wheat1"), name = "HH 95% confidence") %>%
                        layout(title = "Figure 9 - Analysis of Private financial deficit",xaxis = list(title = 'Time'),
                               yaxis = list(title = 'Percentage of GDP'))
                    }else
                      
                      if(input$figure==10){
                        
                        DF <- filter(DF, geo.time == input$geo.time)
                        
                        NLts<- subset(DF , select=c("nl_prv_pc"))
                        NLts <- na.omit(ts(NLts,start = c(1949, 1),frequency = 1))
                        NLfit<-auto.arima(NLts)
                        NLfore<-forecast(NLfit,h=5)
                        
                        PFBts<- subset(DF, select=c("S_pc_disp"))
                        PFBts <- na.omit(ts(PFBts,start = c(1949, 1),frequency = 1))
                        PFBfit<-auto.arima(PFBts)
                        PFBfore<-forecast(PFBfit,h=5)
                        
                        p10 <- plot_ly() %>%
                          add_trace( x = ~DF$time, y = ~DF$nl_prv_pc, type = 'scatter', mode = 'lines', name = "Net Lending")%>%
                          add_trace(x = ~DF$time, y = ~DF$S_pc_disp, type = 'scatter', mode = 'lines', name = 'Private Financial Balance')%>%
                          add_trace(x = time(NLfore$mean), y = NLfore$mean, color = I("navy"), type = 'scatter', mode = 'lines', name = "Net Lend. Prediction", line=list(dash=3, width = 3))%>%
                          add_trace(x = time(PFBfore$mean), y = PFBfore$mean, color = I("darkorange2"),  type = 'scatter', mode = 'lines',name = "Priv Fin Bal Prediction",line=list(dash=3, width = 3))%>%
                          add_ribbons(x = time(NLfore$mean), ymin = NLfore$lower[, 2], ymax = NLfore$upper[, 2],color = I("slategray2"), name = "Net Lend 95% confidence") %>%
                          add_ribbons(x = time(PFBfore$mean), ymin = PFBfore$lower[, 2], ymax = PFBfore$upper[, 2],color = I("wheat1"), name = "Priv Fin Bal 95% confidence") %>%
                          layout(title = "Figure 10 - Private Financial Balance & Net Lending to Priv. Sect",xaxis = list(title = 'Time'),
                                 yaxis = list(title = 'Percentage of Disposable Income'))
                        
                      }else
                        
                        if(input$figure==11){
                          p11 <- plot_ly(M3, x = ~obstime, y = ~obsvalue, type = 'scatter', mode = 'lines', fill = "tozeroy")%>%
                            layout(title = "Figure 11 - M3 growth rate - Eurozone",xaxis = list(title = 'Time'),
                                   yaxis = list(title = 'Percentage'))
                        }
    
  })
  output$plot2<- renderPlotly({
    
    DF <- filter(DF, geo.time == input$geo.time)
    
    
    if(input$figure==2){
      DF <- filter(DF, geo.time == input$geo.time)
      
      p22 <- plot_ly( x = DF$time, y = ~DF$gdp_d, type = 'bar',  name = 'GDP') %>%
        add_trace(x = DF$time, y = DF$AFR_d, name = 'AFR', type = 'bar') %>%
        layout(barmode = 'relative',title = "Changes in Adjusted Fiscal Ratio and GDP",xaxis = list(title = 'Time'),
               yaxis = list(title = 'Percentage Change'))
    }else
      if(input$figure==4){
        
        DF <- filter(DF, geo.time == input$geo.time)
        
        p24 <- plot_ly( x = ~DF$time, y = ~DF$gdp_d, type = 'bar', name = 'GDP') %>%
          add_trace(x = ~DF$time, y = ~DF$ATR_d,  type = 'bar',name = 'ATR')%>%
          layout(barmode = 'relative',title = "Change in Adjusted Trade Ratio and GDP", xaxis = list(title = 'Time'),
                 yaxis = list(title = 'Percentage Change'))
      }else
        if(input$figure==5){
          
          DF <- filter(DF, geo.time == input$geo.time)
          
          p25 <- plot_ly( x = ~DF$time, y = ~DF$AFR_d, type = 'bar', name = 'AFR') %>%
            add_trace(x =  ~DF$time, y = ~DF$CFTR_d, name = 'CFTR',type = 'bar')%>%
            add_trace(  x = ~DF$time, y = ~DF$ATR_d, name = 'ATR',type = 'bar')%>%
            layout(barmode = 'relative',title = "Changes in Ratios",xaxis = list(title = 'Time'),
                   yaxis = list(title = 'Percentage Change'))
        }else
          if(input$figure==6){
            
            DF <- filter(DF, geo.time == input$geo.time)
            
            p26 <- plot_ly( x = ~DF$time, y = ~DF$gdp_d, type = 'bar', name = 'GDP') %>%
              add_trace(x = ~DF$time, y = ~DF$CFTR_d, name = 'CFTR',type = 'bar')%>%
              layout(barmode = 'relative',title = "Change in Combined Fiscal-Trade Ratio and GDP",xaxis = list(title = 'Time'),
                     yaxis = list(title = 'Percentage Change'))
          }else
            if(input$figure==8){
              
              DF <- filter(DF, geo.time == input$geo.time)
              
              p28 <- plot_ly( x = ~DF$time, y = ~DF$disp_inc2_d, type = 'bar', name = 'Real Private Exp.') %>%
                add_trace(x = ~DF$time, y = ~DF$prv_exp_d, name = 'Real Disp. Income',type = 'bar')%>%
                layout(barmode = 'relative',title = "Change in Real Private Expenditure and Disposable Income", xaxis = list(title = 'Time'),
                       yaxis = list(title = 'Percentage Change'))
            }else
              NULL
  })  
  output$plot3 <-renderPlotly({
    
    
    if(input$figure==2){
      
      DF <- filter(DF, geo.time == input$geo.time)
      DF2 <- filter(DF2, geo.time == input$geo2)
      
      p32 <- plot_ly( x = ~DF$time, y = ~DF$AFR_d, type = 'bar', name = input$geo.time) %>%
        add_trace(x = ~DF2$time, y = ~DF2$AFR_d, type = 'bar',name = input$geo2) %>%
        layout(barmode = 'relative',title = "Comparison of Adjusted Fiscal Ratio",xaxis = list(title = 'Time'),
               yaxis = list(title = 'Percentage Change'))
      
    }else
      if(input$figure==4){
        DF <- filter(DF, geo.time == input$geo.time)
        DF2 <- filter(DF2, geo.time == input$geo2)    
        
        p34 <- plot_ly( x = ~DF$time, y = ~DF$ATR_d,type = 'bar', name = input$geo.time) %>%
          add_trace(x = ~DF2$time, y = ~DF2$ATR_d, name = input$geo2,type = 'bar') %>%
          layout(barmode = 'relative',title = "Comparison of Adjusted Trade Ratio",xaxis = list(title = 'Time'),
                 yaxis = list(title = 'Percentage Change'))
        
      }else
        if(input$figure==6){
          DF <- filter(DF, geo.time == input$geo.time)
          DF2 <- filter(DF2, geo.time == input$geo2)
          
          p36 <- plot_ly( x = ~DF$time, y = ~DF$CFTR_d, type = 'bar', name = input$geo.time) %>%
            add_trace(x = ~DF2$time, y = ~DF2$CFTR_d, name = input$geo2,type = 'bar') %>%
            layout(barmode = 'relative',title = "Comparison of Combined Fiscal Trade Ratio",xaxis = list(title = 'Time'),
                   yaxis = list(title = 'Percentage Change'))
        }else
          NULL
  })
  output$GenGov <- renderPlotly({      
    DF <- filter(DF, geo.time == input$country)
    
    GenGov <- plot_ly(DF, x = DF$time, y = DF$gen_gov_balance_pc, type = "bar")%>%
      layout(title = "Figure 1 - General Government Balance", yaxis = list(title = "Percentage of GDP"), xaxis = list(title = "Time"))})
  output$AFRplot <- renderPlotly({        
    DF <- filter(DF, geo.time == input$country)
    
    GDPts<- subset(DF, select=c("gdp"))
    GDPts <- na.omit(ts(GDPts,start = c(1949, 1),frequency = 1))
    GDPfit<-auto.arima(GDPts)
    GDPfore<-forecast(GDPfit,h=5)
    
    AFRts<- subset(DF, select=c("AFR"))
    AFRts <- na.omit(ts(AFRts,start = c(1949, 1),frequency = 1))
    AFRfit<-auto.arima(AFRts)
    AFRfore<-forecast(AFRfit,h=5)
    
    AFRplot<-plot_ly()%>%
      add_lines(x = time(GDPts), y = GDPts,line=list(width = 3), name = 'GDP') %>%
      add_lines(x = time(AFRts), y = AFRts, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'AFR')%>%
      add_trace(x = time(GDPfore$mean), y = GDPfore$mean, color = I("navy"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "GDP pred")%>%
      add_trace(x = time(AFRfore$mean), y = AFRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "AFR pred")%>%
      add_ribbons(x = time(GDPfore$mean), ymin = GDPfore$lower[, 2], ymax = GDPfore$upper[, 2],color = I("slategray2"), name = "GDP 95% Conf")%>%
      add_ribbons(x = time(AFRfore$mean), ymin = AFRfore$lower[, 2], ymax = AFRfore$upper[, 2],color = I("wheat1"), name = "AFR 95% Conf")%>%
      layout(title = "Adjusted Fiscal Ratio and GDP",xaxis = list(title = 'Time'),yaxis = list(title = '€ in Millions'))
    
  })
  output$ATRplot<- renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    GDPts<- subset(DF, select=c("gdp"))
    GDPts <- na.omit(ts(GDPts,start = c(1949, 1),frequency = 1))
    GDPfit<-auto.arima(GDPts)
    GDPfore<-forecast(GDPfit,h=5)
    
    ATRts<- subset(DF, select=c("ATR"))
    ATRts <- na.omit(ts(ATRts,start = c(1949, 1),frequency = 1))
    ATRfit<-auto.arima(ATRts)
    ATRfore<-forecast(ATRfit,h=5)
    
    ATRplot<-plot_ly()%>%
      add_lines(x = time(GDPts), y = GDPts, line=list(width = 3),name = 'GDP') %>%
      add_lines(x = time(ATRts), y = ATRts, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'ATR')%>%
      add_trace(x = time(GDPfore$mean), y = GDPfore$mean, color = I("navy"),type = 'scatter', mode = 'lines',line=list(dash=3, width = 3), name = "GDP pred")%>%
      add_trace(x = time(ATRfore$mean), y = ATRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "ATR pred")%>%
      add_ribbons(x = time(GDPfore$mean), ymin = GDPfore$lower[, 2], ymax = GDPfore$upper[, 2],color = I("slategray2"), name = "GDP 95% Conf") %>%
      add_ribbons(x = time(ATRfore$mean), ymin = ATRfore$lower[, 2], ymax = ATRfore$upper[, 2],color = I("wheat1"), name = "AFR 95% Conf") %>%
      layout(title = "Adjusted Trade Ratio and GDP", xaxis = list(title = 'Time'),
             yaxis = list(title = '€ in Millions')) })
  output$CFTRplot<- renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    GDPts<- subset(DF, select=c("gdp"))
    GDPts <- na.omit(ts(GDPts,start = c(1949, 1),frequency = 1))
    GDPfit<-auto.arima(GDPts)
    GDPfore<-forecast(GDPfit,h=5)
    
    CFTRts<- subset(DF, select=c("CFTR"))
    CFTRts <- na.omit(ts(CFTRts,start = c(1949, 1),frequency = 1))  
    CFTRfit<-auto.arima(CFTRts)
    CFTRfore<-forecast(CFTRfit,h=5)
    
    CFTRplot<-plot_ly()%>%
      add_lines(x = time(GDPts), y = GDPts, line=list(width = 3),name = 'GDP') %>%
      add_lines(x = time(CFTRts), y = CFTRts, type = 'scatter', line=list(width = 3),mode = 'lines', name = 'CFTR')%>%
      add_trace(x = time(GDPfore$mean), y = GDPfore$mean, color = I("navy"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "GDP pred.")%>%
      add_trace(x = time(CFTRfore$mean), y = CFTRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "CFTR pred.")%>%
      add_ribbons(x = time(GDPfore$mean), ymin = GDPfore$lower[, 2], ymax = GDPfore$upper[, 2],color = I("slategray2"), name = "GDP 95% Conf.") %>%
      add_ribbons(x = time(CFTRfore$mean), ymin = CFTRfore$lower[, 2], ymax = CFTRfore$upper[, 2],color = I("wheat1"), name = "CFTR 95% Conf.") %>%
      layout(title = "Combined Fiscal-Trade Ratio and GDP",xaxis = list(title = 'Time'),
             yaxis = list(title = '€ in Millions')) 
    
    
  })
  output$CFTRplot2<- renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    AFRts<- subset(DF, select=c("AFR"))
    AFRts <- na.omit(ts(AFRts,start = c(1949, 1),frequency = 1))
    AFRfit<-auto.arima(AFRts)
    AFRfore<-forecast(AFRfit,h=5)
    
    ATRts<- subset(DF, select=c("ATR"))
    ATRts <- na.omit(ts(ATRts,start = c(1949, 1),frequency = 1))
    ATRfit<-auto.arima(ATRts)
    ATRfore<-forecast(ATRfit,h=5)
    
    CFTRts<- subset(DF, select=c("CFTR"))
    CFTRts <- na.omit(ts(CFTRts,start = c(1949, 1),frequency = 1))  
    CFTRfit<-auto.arima(CFTRts)
    CFTRfore<-forecast(CFTRfit,h=5)
    
    CFTRplot2<-plot_ly()%>%
      add_lines(x = time(AFRts), y = AFRts,line=list(width = 3), name = 'AFR') %>%
      add_lines(x = time(ATRts), y = ATRts, type = 'scatter', line=list(width = 3),mode = 'lines', name = 'ATR')%>%
      add_lines(x = time(CFTRts), y = CFTRts, type = 'scatter', line=list(width = 3),mode = 'lines', name = 'CFTR')%>%
      add_trace(x = time(AFRfore$mean), y = AFRfore$mean, color = I("navy"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "AFR Prediction")%>%
      add_trace(x = time(ATRfore$mean), y = ATRfore$mean, color = I("darkorange2"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "ATR Prediction")%>%
      add_trace(x = time(CFTRfore$mean), y = CFTRfore$mean, color = I("darkgreen"), type = 'scatter', mode = 'lines',line=list(dash=3, width = 3),name = "CFTR Prediction")%>%
      #add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],color = I("slategray2"), name = "GDP 95% confidence") %>%
      #add_ribbons(x = time(fore2$mean), ymin = fore2$lower[, 2], ymax = fore2$upper[, 2],color = I("wheat1"), name = "AFR 95% confidence") %>%
      layout(title = "Combined Fiscal-Trade Ratio",xaxis = list(title = 'Time'),
             yaxis = list(title = '€ in Millions'))        })
  output$BOPplot<- renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    BOPplot <- plot_ly( x = ~DF$time, y = ~DF$bop_pc, type = 'scatter', mode = 'lines',line=list(width = 3), name = 'Current BOP')%>%
      add_trace(x = ~DF$time, y = DF$bop_goods_pc, type = 'scatter', mode = 'lines',line=list(width = 3),name = 'BOP Goods')%>%
      add_trace(x = ~DF$time, y = ~DF$bop_soods_pc, type = 'scatter', mode = 'lines',line=list(width = 3),name = 'BOP Services')%>%
      add_trace(x = ~DF$time, y = ~DF$pb_bal_pc, line=list(width = 3),name = 'Income Balances')%>%
      layout(title = "Current Balance of Payments",xaxis = list(title = 'Time'),
             yaxis = list(title = 'Percentage of GDP'))
    
  })
  output$DispIncplot <-renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    disp_incts<- subset(DF, select=c("disp_inc"))
    disp_incts <- na.omit(ts(disp_incts,start = c(1949, 1),frequency = 1))
    DIfit<-auto.arima(disp_incts)
    DIfore<-forecast(DIfit,h=5)
    
    PrvExpTs<- subset(DF, select=c("prv_exp"))
    PrvExpTs <- na.omit(ts(PrvExpTs,start = c(1949, 1),frequency = 1))
    PEfit<-auto.arima(PrvExpTs)
    PEfore<-forecast(PEfit,h=5)
    
    
    
    DispIncplot <- plot_ly() %>%
      add_trace(x = ~DF$time, y = ~DF$disp_inc, type = 'scatter', mode = 'lines', name = 'Private Exp')%>%  #fill='tozeroy'#
      add_trace(x = ~DF$time, y = ~DF$prv_exp, type = 'scatter', mode = 'lines',name = 'Disp Income')%>% #, fill='tozeroy'
      add_trace(x = time(DIfore$mean), y = DIfore$mean, color = I("navy"), name = "Priv Exp Pred",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
      add_trace(x = time(PEfore$mean), y = PEfore$mean, color = I("darkorange2"), name = "Disp Inc Pred",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
      add_ribbons(x = time(DIfore$mean), ymin = DIfore$lower[, 2], ymax = DIfore$upper[, 2],color = I("slategray2"), name = "Priv Exp 95% Conf") %>%
      add_ribbons(x = time(PEfore$mean), ymin = PEfore$lower[, 2], ymax = PEfore$upper[, 2],color = I("wheat1"), name = "Disp Inc 95% Conf") %>%
      layout(title = "Real Private Expenditure and Disposable Income", xaxis = list(title = 'Time'),
             yaxis = list(title = '€ in Millions'))
    
  })
  output$SavInv <- renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    NFCts<- subset(DF, select=c("nfc_sav_m_inv_pc"))
    NFCts <- na.omit(ts(NFCts,start = c(1949, 1),frequency = 1))
    NFCfit<-auto.arima(NFCts)
    NFCfore<-forecast(NFCfit,h=5)
    
    HHts<- subset(DF, select=c("hh_sav_m_inv_pc"))
    HHts <- na.omit(ts(HHts,start = c(1949, 1),frequency = 1))
    HHfit<-auto.arima(HHts)
    HHfore<-forecast(HHfit,h=5)
    
    SavInv <- plot_ly() %>%
      add_trace( x = ~DF$time, y = ~DF$nfc_sav_m_inv_pc, type = 'scatter', mode = 'lines', name = "NFC SAV-INV") %>%
      add_trace(x = ~DF$time, y = ~DF$hh_sav_m_inv_pc,type = 'scatter', mode = 'lines', name = 'HH SAV-INV')%>%
      add_trace(x = time(NFCfore$mean), y = NFCfore$mean, color = I("navy"), name = "NFC Pred",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
      add_trace(x = time(HHfore$mean), y = HHfore$mean, color = I("darkorange2"), name = "HH Pred",type = 'scatter', mode = 'lines',line=list(dash=3, width = 3))%>%
      add_ribbons(x = time(NFCfore$mean), ymin = NFCfore$lower[, 2], ymax = NFCfore$upper[, 2],color = I("slategray2"), name = "NFC 95% Conf") %>%
      add_ribbons(x = time(HHfore$mean), ymin = HHfore$lower[, 2], ymax = HHfore$upper[, 2],color = I("wheat1"), name = "HH 95% Conf") %>%
      layout(title = "Analysis of Private Financial Deficit",xaxis = list(title = 'Time'),
             yaxis = list(title = 'Percentage of GDP'))
    
    
    
  })
  output$PrivBalplot <-renderPlotly({
    DF <- filter(DF, geo.time == input$country)
    
    NLts<- subset(DF , select=c("nl_prv_pc"))
    NLts <- na.omit(ts(NLts,start = c(1949, 1),frequency = 1))
    NLfit<-auto.arima(NLts)
    NLfore<-forecast(NLfit,h=5)
    
    PFBts<- subset(DF, select=c("S_pc_disp"))
    PFBts <- na.omit(ts(PFBts,start = c(1949, 1),frequency = 1))
    PFBfit<-auto.arima(PFBts)
    PFBfore<-forecast(PFBfit,h=5)
    
    PrivBalplot <- plot_ly() %>%
      add_trace( x = ~DF$time, y = ~DF$nl_prv_pc, type = 'scatter', mode = 'lines', name = "Net Lending")%>%
      add_trace(x = ~DF$time, y = ~DF$S_pc_disp, type = 'scatter', mode = 'lines', name = 'Private Fin Bal')%>%
      add_trace(x = time(NLfore$mean), y = NLfore$mean, color = I("navy"), type = 'scatter', mode = 'lines', name = "Net Lend Pred", line=list(dash=3, width = 3))%>%
      add_trace(x = time(PFBfore$mean), y = PFBfore$mean, color = I("darkorange2"),  type = 'scatter', mode = 'lines',name = "Fin Bal Pred",line=list(dash=3, width = 3))%>%
      add_ribbons(x = time(NLfore$mean), ymin = NLfore$lower[, 2], ymax = NLfore$upper[, 2],color = I("slategray2"), name = "NL 95% Conf") %>%
      add_ribbons(x = time(PFBfore$mean), ymin = PFBfore$lower[, 2], ymax = PFBfore$upper[, 2],color = I("wheat1"), name = "PFB 95% Conf" ) %>%
      layout(title = "Private Financial Balance & Net Lending to Priv. Sect",xaxis = list(title = 'Time'),
             yaxis = list(title = 'Percentage of Disposable Income'))
    
  })
  
  output$boxtitle<-renderText({
    if(input$figure==1){
      "General Government Balance"
    }else{
      if(input$figure==2){
        "Fiscal Ratio and GDP"
      }else{
        if(input$figure==3){
          "Current Balance of Payments"
        }else{
          if(input$figure==4){
            "Trade Ratio and GDP"
          }else{
            if(input$figure==5){
              "Combined Fiscal-Trade Ratio"
            }else{
              if(input$figure==6){
                "Combined Fiscal-Trade Ratio and GDP"
              }else{
                if(input$figure==7){
                  "Sectoral Balances"
                }else{
                  if(input$figure==8){
                    "Private Expenditure and Disposable Income"
                  }else{
                    if(input$figure==9){
                      "The Private Financial Deficit"
                    }else{
                      if(input$figure==10){
                        "Net Financial Lending and Private Balances"
                      }else{
                        if(input$figure==11){
                          "Money Stock (M3)"
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  
  output$text<-renderText({
    if(input$figure==1){
      "This figure depicts the Government surplus/deficit as a percentage of GDP. The budget is said to be expansionary when in deficit, restrictive when in surplus and neutral when close to zero over a period of time."
    } else{
      if(input$figure==2){
        "The Fiscal Ratio (FR) is defined in Godley's paper as G/THETA, where G is government spending and THETA=T/Y (Tax receipts over output). The ratio should be equal to GDP when the budget is balanced, be greater in a deficit and be less than GDP when a government surplus exists. In these graphs, we also use growth rates to see if both the FR and GDP are tracking each other."      
      }else{
        if(input$figure==3){
          "The figure below shows the Balance of Payments for the selected country, with breakdowns for goods, services and income balances. Positive values indicate that income flows into the selected country from the rest of the world, while negative values indicate that income is leaving the country."
        }else{
          if(input$figure==4){
            "The Trade Ratio (TR) is defined as X/MU where X is exports and MU is the import propensity of the economy. The TR will exceed GDP if the balance of payments is positive and be lower than GDP if the balance of payments is negative. In this way, the TR can be used to crudely measure trade imbalances for the selected country. As with the Fiscal ratio, we use growth rates to see if TR and GDP track each other."          
          }else{
            if(input$figure==5){
              "These graphs depicts the Combined Fiscal and Trade Ratio (CFTR) alongside the TR and FR series. The CFTR shows us the extent to which government spending together with exports has contributed to GDP, relative to how much taxation and imports took out of the economy."
            }else{
              if(input$figure==6){
                "These figures depicts the growth rates for CFTR and GDP, they should track to a degree. We can look at the economy as a bathtub, with government expenditure and exports flowing in, and taxation acting as an outflow together with imports. If GDP remains level, it means that inflows equal outflows and that GDP equals the CFTR. We can use these graphs to check if any imbalances exist between the CFTR and GDP."
              }else{
                if(input$figure==7){
                  "The three major financial balances depict the sum of all non-financial flows for the private sector, the government and the rest of the world, named the Private Financial Balance, the Government Balance and the Current Balance of Payments, respectively. These three balances must by identity sum to zero. The government balance here is depicted such that positive values imply a deficit, to more clearly show that a private deficit is equal to a government surplus and a balance of payments deficits. This identity tells us that government deficits and balance of payments surpluses increase private income and net wealth, while budget surpluses and balance of payments deficits take income out from the private sector and decrease wealth."
                }else{
                  if(input$figure==8){
                    "This figure shows private disposable income and expenditure, in real terms."
                  }else{
                    if(input$figure==9){
                      "Figure 9 shows savings minus fixed investment for businesses and households, which is a core component of the private financial balance. For households, a major component of investment is the purchase of new housing or renovation of existing housing. Deficits in this figure can imply excessive consumption and/or housing investment in the household sector. Business investment is in fixed assets such as buildings, machinery, etc. Surpluses show that undistributed profits exceed investment."
                    }else{
                      if(input$figure==10){
                        "This figure depicts the Private Financial Balance and Net Lending to the non-financial private sector for the selected country, as a percentage of disposable income. In the national accounts, these have an inverse relationship, as deficits in the non-financial balance must be covered by increased borrowing in the financial account. Surpluses are used to increase net worth, leading to a decrease in net lending."
                      }else{
                        if(input$figure==11){
                          "This figure depicts the growth rate of the money stock for the Eurozone. When taken together with net lending, its growth implies an increase in indebtedness."
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  
})
