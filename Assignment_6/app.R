# BDA400 Assignment 6 - Technical Analysis Visualization
# Student: Sumandeep Singh Brar
# Data: Yahoo Finance via quantmod
# Run: install.packages(c('shiny','ggplot2','quantmod','TTR')); shiny::runApp()
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  titlePanel('Stock Portfolio Technical Analysis Dashboard'),
  sidebarLayout(
    sidebarPanel(
      textInput('symbol','Yahoo Finance ticker','AAPL'),
      dateRangeInput('dates','Date range',start=Sys.Date()-365,end=Sys.Date()),
      selectInput('frame','Time frame',c('Daily','Weekly','Monthly')),
      selectInput('chart','Chart type',c('Line','Area','Candlestick')),
      checkboxGroupInput('indicators','Technical indicators',c('Moving Averages','RSI','MACD'),selected=c('Moving Averages','RSI','MACD')),
      numericInput('short','Short moving average',20,min=2,max=200),
      numericInput('long','Long moving average',50,min=3,max=300),
      checkboxInput('signals','Show crossover BUY / SELL annotations',TRUE),
      helpText('Signals are illustrative, not financial advice. RSI and MACD use separate panels to preserve their scales.')
    ),
    mainPanel(textOutput('status'),plotOutput('stock_chart',height='420px'),
              conditionalPanel("input.indicators.includes('RSI')",plotOutput('rsi_chart',height='190px')),
              conditionalPanel("input.indicators.includes('MACD')",plotOutput('macd_chart',height='190px')),
              h4('Recent signals'),tableOutput('signal_table'))
  )
)

server <- function(input,output,session) {
  prices <- reactive({
    req(nzchar(trimws(input$symbol)), input$dates)
    validate(need(input$short < input$long,'Short MA must be smaller than long MA.'))
    sym <- toupper(trimws(input$symbol))
    raw <- tryCatch(getSymbols(sym,src='yahoo',from=as.Date(input$dates[1])-max(150, input$long*4),to=as.Date(input$dates[2])+1,auto.assign=FALSE),error=function(e) NULL)
    validate(need(!is.null(raw) && NROW(raw)>input$long+5,'Unable to load sufficient stock history. Check symbol, dates, or internet.'))
    raw <- na.omit(raw)
    if(input$frame=='Weekly') raw <- to.weekly(raw,indexAt='lastof',drop.time=TRUE)
    if(input$frame=='Monthly') raw <- to.monthly(raw,indexAt='lastof',drop.time=TRUE)
    validate(need(NROW(raw)>input$long+2,'Not enough observations for selected moving averages; choose a longer range or shorter periods.'))
    close <- as.numeric(Cl(raw))
    sma_s <- as.numeric(SMA(close,n=input$short)); sma_l <- as.numeric(SMA(close,n=input$long))
    rsi <- as.numeric(RSI(close,n=14))
    macd <- MACD(close,nFast=12,nSlow=26,nSig=9,maType='EMA',percent=FALSE)
    state <- ifelse(sma_s>sma_l,'Buy',ifelse(sma_s<sma_l,'Sell','Hold'))
    event <- rep('Hold',length(state))
    for(i in 2:length(state)) if(!is.na(sma_s[i]) && !is.na(sma_l[i]) && !is.na(sma_s[i-1]) && !is.na(sma_l[i-1])) {
      if(sma_s[i]>sma_l[i] && sma_s[i-1]<=sma_l[i-1]) event[i]<-'BUY'
      if(sma_s[i]<sma_l[i] && sma_s[i-1]>=sma_l[i-1]) event[i]<-'SELL'
    }
    d <- data.frame(Date=as.Date(index(raw)),Open=as.numeric(Op(raw)),High=as.numeric(Hi(raw)),Low=as.numeric(Lo(raw)),Close=close,ShortMA=sma_s,LongMA=sma_l,RSI=rsi,MACD=as.numeric(macd[,1]),MACDSignal=as.numeric(macd[,2]),State=state,Event=event)
    d[d$Date>=as.Date(input$dates[1]) & d$Date<=as.Date(input$dates[2]),,drop=FALSE]
  })
  output$status <- renderText({d<-prices(); paste(nrow(d),'observations |',toupper(input$symbol),'|',input$frame)})
  output$stock_chart <- renderPlot({
    d<-prices();validate(need(nrow(d)>0,'No observations in selected range.'))
    p<-ggplot(d,aes(x=Date))
    if(input$chart=='Line') p<-p+geom_line(aes(y=Close),color='#2463a6',linewidth=.8)
    if(input$chart=='Area') p<-p+geom_area(aes(y=Close),fill='#91c5ed',alpha=.6)+geom_line(aes(y=Close),color='#2463a6')
    if(input$chart=='Candlestick') {
      d$Direction<-ifelse(d$Close>=d$Open,'Up','Down')
      width<-if(input$frame=='Monthly') 17 else if(input$frame=='Weekly') 4 else .65
      p<-p+geom_segment(data=d,aes(x=Date,xend=Date,y=Low,yend=High,color=Direction))+geom_rect(data=d,aes(xmin=Date-width/2,xmax=Date+width/2,ymin=pmin(Open,Close),ymax=pmax(Open,Close),fill=Direction),color='grey30')+scale_fill_manual(values=c(Down='#dc6262',Up='#48a37c'))+scale_color_manual(values=c(Down='#dc6262',Up='#48a37c'))
    }
    if('Moving Averages'%in%input$indicators) p<-p+geom_line(aes(y=ShortMA,color='Short MA'),linewidth=.7,na.rm=TRUE)+geom_line(aes(y=LongMA,color='Long MA'),linewidth=.7,na.rm=TRUE)+scale_color_manual(values=c('Short MA'='#f09b29','Long MA'='#784cb2'))
    if(isTRUE(input$signals)) p<-p+geom_point(data=subset(d,Event!='Hold'),aes(y=Close,shape=Event),size=3,color='#222222')+geom_text(data=subset(d,Event!='Hold'),aes(y=Close,label=Event),vjust=-1,size=3)
    p+labs(title=paste(toupper(input$symbol),'Price & Trading Signals'),y='Price (USD)',x=NULL)+theme_minimal(base_size=12)
  })
  output$rsi_chart<-renderPlot({d<-prices();ggplot(d,aes(Date,RSI))+geom_line(color='#704db0',na.rm=TRUE)+geom_hline(yintercept=c(30,70),linetype='dashed',color='grey50')+coord_cartesian(ylim=c(0,100))+labs(title='RSI (14)',x=NULL,y='RSI')+theme_minimal()})
  output$macd_chart<-renderPlot({d<-prices();ggplot(d,aes(Date))+geom_col(aes(y=MACD-MACDSignal),fill='#a9c6d8',na.rm=TRUE)+geom_line(aes(y=MACD,color='MACD'),na.rm=TRUE)+geom_line(aes(y=MACDSignal,color='Signal'),na.rm=TRUE)+scale_color_manual(values=c(MACD='#2463a6',Signal='#dc6262'))+labs(title='MACD (12,26,9)',x=NULL,y='MACD',color=NULL)+theme_minimal()})
  output$signal_table<-renderTable({d<-prices();tail(d[d$Event!='Hold',c('Date','Close','Event')],10)},digits=2)
}
shinyApp(ui,server)
