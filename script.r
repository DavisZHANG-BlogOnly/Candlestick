source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly");
libraryRequireInstall("showtext");
####################################################
powerbi_rEnableShowText =  1
powerbi_rEnableShowTextForCJKLanguages =  1
#showtext_auto(enable = TRUE)
#Sys.setlocale("LC_ALL","CHS")

CandleStyle <- "classical"
if(exists("candlestick_CandleStyle")){
   CandleStyle <- candlestick_CandleStyle
}

GoodColor <- "green"
if(exists("candlestick_GoodColor")){
   GoodColor <- candlestick_GoodColor
}

BadColor <- "red"
if(exists("candlestick_BadColor")){
   BadColor <- candlestick_BadColor
}

LineColor_1 <- "#FF00FF"
if(exists("lines_ma_customized_color")){
   LineColor_1 <- lines_ma_customized_color
}

li_ma05 = FALSE
if(exists("lines_ma05")){
    li_ma05 <- lines_ma05
}

li_ma10 = FALSE
if(exists("lines_ma10")){
    li_ma10 <- lines_ma10
}

li_ma20 = FALSE
if(exists("lines_ma20")){
    li_ma20 <- lines_ma20
}

li_ma30 = FALSE
if(exists("lines_ma30")){
    li_ma30 <- lines_ma30
}

li_ma60 = FALSE
if(exists("lines_ma60")){
    li_ma60 <- lines_ma60
}

li_ma_c = FALSE
if(exists("lines_ma_customized")){
    li_ma_c <- lines_ma_customized
}

li_ma_i = 7
if(exists("lines_ma_customized_items")){
    li_ma_i <- lines_ma_customized_items
}

ma_custom_name = NULL
if(exists("lines_ma_customized_name")){
    ma_custom_name <- lines_ma_customized_name
}

xFonts <- "Arial"
if(exists("xAxis_Fonts")){
   xFonts <- xAxis_Fonts
}

xSize <- 8
if(exists("xAxis_Size")){
   xSize <- xAxis_Size
}

xColor <- "#FF2435"
if(exists("xAxis_Color")){
   xColor <- xAxis_Color
}

Format <- "%Y-%m-%d"
if(exists("xAxis_Format")){
    Format <- xAxis_Format
}

xGridColor <- "#FF2435"
if(exists("xAxis_GridColor")){
   xGridColor <- xAxis_GridColor
}

xGridStyle <- "3"
if(exists("xAxis_GridStyle")){
   xGridStyle <- xAxis_GridStyle
}

yFonts <- "Arial"
if(exists("yAxis_Fonts")){
   yFonts <- yAxis_Fonts
}

ySize <- 8
if(exists("yAxis_Size")){
   ySize <- yAxis_Size
}

yColor <- "#FF2435"
if(exists("yAxis_Color")){
   yColor <- yAxis_Color
}

yGridColor <- "#FF2435"
if(exists("yAxis_GridColor")){
   yGridColor <- yAxis_GridColor
}

yGridStyle <- "3"
if(exists("yAxis_GridStyle")){
   yGridStyle <- yAxis_GridStyle
}

Separator <- FALSE
if(exists("yAxis_Separator")){
    Separator <- yAxis_Separator
}

yMax <- max(hi,na.rm=T)
if(exists("yAxis_yMax")){
   yMax <- yAxis_yMax
}

yMin <- min(lo,na.rm=T)
if(exists("yAxis_yMin")){
   yMin <- yAxis_yMin
}

show_latest_cl <- FALSE
if(exists("advance_show_latest_cl")){
   show_latest_cl <- advance_show_latest_cl
} 

show_Hover <- TRUE
if(exists("hover_show")){
    show_Hover <- hover_show
}

show_Date <- FALSE
if(exists("hover_show_Date")){
    show_Date <- hover_show_Date
}

show_MA <- FALSE
if(exists("hover_show_MA")){
    show_MA <- hover_show_MA
}

h_f <- "Arial"
if(exists("hover_Fonts")){
   h_f <- hover_Fonts
}

h_f_Color <- "white"
if(exists("hover_Fonts_Color")){
   h_f_Color <- hover_Fonts_Color
}

h_f_Size <- 12
if(exists("hover_Size")){
   h_f_Size <- hover_Size
}


h_Bg_Color <- "#232F34"
if(exists("hover_Bg_Color")){
   h_bg_Color <- hover_Bg_Color
}

h_Border_Color <- "#232F34"
if(exists("hover_Border_Color")){
   h_Border_Color <- hover_Border_Color
}

display_bar = 'H'
if(exists("modeBar_display")){
    display_bar <- modeBar_display
}
################### Function ####################
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 3, partAvailable = 1)
{
  # partAvailable, wich portion of window is available, in [0,1]
  if(is.null(strText))
    return (NULL)
  
  SCL = 0.075*strCex/0.8
  pardin = par()$din
  gStand = partAvailable*(isH*pardin[1]+(1-isH)*pardin[2]) /SCL
  
  # if very very long abbreviate
  if(nchar(strText)>abbrTo && nchar(strText)> 1)
    strText = abbreviate(strText, abbrTo)
  
  # if looooooong convert to lo...
  if(nchar(strText)>round(gStand) && nchar(strText)> 1)
    strText = paste(substring(strText,1,floor(gStand)),"...",sep="")
  
  # if shorter than maxChar remove 
  if(gStand<=maxChar)
    strText = NULL
  
  return(strText) 
}

#This code should be added before unlist()
da_names <- names(da)[1]
cl_names <- names(cl)[1]
op_names <- names(op)[1]
hi_names <- names(hi)[1]
lo_names <- names(lo)[1]
################### Actual code ####################
#g = qplot(`Petal.Length`, data = iris, fill = `Species`, main = Sys.time());
if(class(cl)!="numeric"){
    cl <- as.numeric(unlist(cl))
} else {
    cl <- unlist(cl)
}

if(class(op)!="numeric"){
    op <- as.numeric(unlist(op))
} else {
    op <- unlist(op)
}

if(class(hi)!="numeric"){
    hi <- as.numeric(unlist(hi))
} else {
    hi <- unlist(hi)
}

if(class(lo)!="numeric"){
    lo <- as.numeric(unlist(lo))
} else {
    lo <- unlist(lo)
}

if(exists("li")){
  if(is.null(ncol(li))){
    if(class(li)!="numeric"){
      li_1 <- as.numeric(unlist(li))
    } else {
      li_1 <- unlist(li)
    }
  } else if(ncol(li)==2){
    if(class(li[,1])!="numeric"){
      li_1 <- as.numeric(unlist(li[,1]))
    } else {
      li_1 <- unlist(li[,1])
    }
    if(class(li[,2])!="numeric"){
      li_2 <- as.numeric(unlist(li[,2]))
    } else {
      li_2 <- unlist(li[,2])
    }
  } else if(ncol(li)==3){
    if(class(li[,1])!="numeric"){
      li_1 <- as.numeric(unlist(li[,1]))
    } else {
      li_1 <- unlist(li[,1])
    }
    if(class(li[,2])!="numeric"){
      li_2 <- as.numeric(unlist(li[,2]))
    } else {
      li_2 <- unlist(li[,2])
    }
    if(class(li[,3])!="numeric"){
      li_3 <- as.numeric(unlist(li[,3]))
    } else {
      li_3 <- unlist(li[,3])
    }
  } else if(ncol(li)==4){
    if(class(li[,1])!="numeric"){
      li_1 <- as.numeric(unlist(li[,1]))
    } else {
      li_1 <- unlist(li[,1])
    }
    if(class(li[,2])!="numeric"){
      li_2 <- as.numeric(unlist(li[,2]))
    } else {
      li_2 <- unlist(li[,2])
    }
    if(class(li[,3])!="numeric"){
      li_3 <- as.numeric(unlist(li[,3]))
    } else {
      li_3 <- unlist(li[,3])
    }
    if(class(li[,4])!="numeric"){
      li_4 <- as.numeric(unlist(li[,4]))
    } else {
      li_4 <- unlist(li[,4])
    }
  }
} else {
  li = NULL
}




yIndex <- round(seq(from=yMin,to=yMax,length=5),2)

if(nrow(da)>10){
   xIndex <- round(seq(from=1,to=nrow(da),length=5)) 
} else {
   xIndex <- round(seq(from=1,to=nrow(da),length=nrow(da)))
}


id <- 1:nrow(da)


if(class(da)!="Date"){
    da <- as.Date(unlist(da))
} else {
    da <- unlist(da)
}

if(is.null(li)){
  dt <- data.frame(id,da,cl,op,hi,lo,stringsAsFactors=FALSE)
} else if(ncol(li)==1){
  dt <- data.frame(id,da,cl,op,hi,lo,li_1,stringsAsFactors=FALSE)
} else if(ncol(li)==2){
  dt <- data.frame(id,da,cl,op,hi,lo,li_1,li_2,stringsAsFactors=FALSE)
} else if(ncol(li)==3){
  dt <- data.frame(id,da,cl,op,hi,lo,li_1,li_2,li_3,stringsAsFactors=FALSE)
} else if(ncol(li)>=4){
  dt <- data.frame(id,da,cl,op,hi,lo,li_1,li_2,li_3,li_4,stringsAsFactors=FALSE)
}



dt <- dt[!(dt$cl==0 & dt$op==0 & dt$lo==0 & dt$hi==0),]
####################################################
detach("package:plotly", unload = TRUE)
ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 1)}



if(li_ma05){
    dt$MA05 <- ma(dt$cl,5)
} 

if(li_ma10){
    dt$MA10 <- ma(dt$cl,10)
} 

if(li_ma20){
    dt$MA20 <- ma(dt$cl,20)
} 

if(li_ma30){
    dt$MA30 <- ma(dt$cl,30)
} 

if(li_ma60){
    dt$MA60 <- ma(dt$cl,60)
} 

if(li_ma_c){
    dt$MA_C <- assign(paste("MA", li_ma_i, sep = ""), ma(dt$cl,li_ma_i))
} 

####################################################
library(plotly)
dt$candleLow <- pmin(op,cl)
dt$candleUp <- pmax(op,cl)
dt$candleMid <- cl
dt$col <- GoodColor
dt$col[dt$cl<dt$op] <- BadColor


candleWidth <- 0.35
lineWidth <- 0.25
trendlineWidth <- 0.30
trendlineNames <- names(li[1])
closeNames <- names(cl[1])
dateNames <- names(da[1])


if(xSize<5){
    xSize <- 5
} else if(xSize > 20){
    xSize <- 20
}

if(ySize<5){
    ySize <- 5
} else if(ySize > 20){
    ySize <- 20
}

if(is.null(ma_custom_name)){
   if(li_ma_i<10){
       ma_custom_name <- paste("MA",0,li_ma_i, sep ="")
   } else {
       ma_custom_name <- paste("MA",li_ma_i, sep ="")
   }
}
####################################################
g <- ggplot(dt,aes(x=id))

xLab <- format(dt$da[c(xIndex)],Format)

if(Separator){
    yLab <- format(yIndex,big.mark=",")
} else {
    yLab <- yIndex
}

g <- g + scale_x_continuous(breaks=xIndex,labels=xLab) 

#g <- g + scale_x_continuous(breaks=xIndex,labels=xLab) + 
#  scale_y_continuous(breaks=yIndex,labels=yLab) #this is "fixed y-axis label" version, the plot will generate auto axis by plotly if remove it.

if (exists("xAxis_show") && xAxis_show!=TRUE){
  g <- g + theme(
    axis.text.x=element_blank()
  )
} else {
  g <- g + theme(
  axis.text.x = element_text(color = xColor,family = xFonts
  ,size = xSize, angle = 0, hjust = 0, vjust = 0, face = "plain")
  )
}

# Customize ggplot2 axis labels with different colors will not working after it transfer to plotly object.

if (exists("yAxis_show") && yAxis_show!=TRUE){
  g <- g + theme(
    axis.text.y=element_blank()
  )
} else {
  g <- g + theme(
  axis.text.y = element_text(color = yColor,family = yFonts,size = ySize, angle = 0, hjust = 0, vjust = 0, face = "plain"
  )
  )
}

g <- g + geom_line(data = dt,color="transparent",size=trendlineWidth,aes(x=id,y=cl))

layers <- 2

if (exists("xAxis_Grid") && xAxis_Grid==TRUE){
  layers <- layers + 1
  g <- g + geom_vline(xintercept=xIndex,size=0.3,color=xGridColor,linetype=xGridStyle)
}

if (exists("yAxis_Grid") && yAxis_Grid==TRUE){
  layers <- layers + 1
  g <- g + geom_hline(yintercept=yIndex,size=0.3,color=yGridColor,linetype=yGridStyle)
}



if(CandleStyle=="classical"){
  g <- g +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=candleUp,ymax=hi)) +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=lo,ymax=candleLow)) +
  geom_rect(data = dt,color=dt$col,fill=dt$col,size=lineWidth,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=candleLow,ymax=candleUp))+
  guides(fill=FALSE)
  layers <- layers + 5
} else if (CandleStyle=="caps"){
  g <- g +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=candleUp,ymax=hi)) +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=lo,ymax=candleLow)) +
  geom_rect(data = dt,color=dt$col,size=lineWidth,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=lo,ymax=lo)) + 
  geom_rect(data = dt,color=dt$col,size=lineWidth,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=hi,ymax=hi)) + 
  geom_rect(data = dt,color=dt$col,size=lineWidth,fill=dt$col,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=candleLow,ymax=candleUp))+
  guides(fill=FALSE) 
  layers <- layers + 9  
} else if (CandleStyle=="yin-yang"){
  dt$fill <- BadColor
  dt$fill[dt$cl>dt$op] <- "transparent"
  g <- g +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=candleUp,ymax=hi)) +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=lo,ymax=candleLow)) +
  geom_rect(data = dt,color=dt$col,size=lineWidth,fill=dt$fill,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=candleLow,ymax=candleUp))+
  guides(fill=FALSE)  
  layers <- layers + 6   
} else if (CandleStyle=="yin-yang with caps"){
  dt$fill <- BadColor
  dt$fill[dt$cl>dt$op] <- "transparent"
  g <- g +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=candleUp,ymax=hi)) +
  geom_linerange(data = dt,color=dt$col,size=lineWidth,aes(ymin=lo,ymax=candleLow)) +
  geom_rect(data = dt,color=dt$col,size=lineWidth,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=lo,ymax=lo)) + 
  geom_rect(data = dt,color=dt$col,size=lineWidth,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=hi,ymax=hi)) +
  geom_rect(data = dt,color=dt$col,fill=dt$fill,size=lineWidth,aes(xmin=id-candleWidth,xmax=id+candleWidth,ymin=candleLow,ymax=candleUp))+
  guides(fill=FALSE)   
  layers <- layers + 10 
}





if(li_ma05){
  g <- g + geom_line(data = dt,color="#FFFAF0",size=trendlineWidth,aes(x=id,y=MA05))
  layers <- layers + 1
}

if(li_ma10){
  g <- g + geom_line(data = dt,color="yellow",size=trendlineWidth,aes(x=id,y=MA10))
  layers <- layers + 1
}

if(li_ma20){
  g <- g + geom_line(data = dt,color="#8A2BE2",size=trendlineWidth,aes(x=id,y=MA20))
  layers <- layers + 1
}

if(li_ma30){
  g <- g + geom_line(data = dt,color="green",size=trendlineWidth,aes(x=id,y=MA30))
  layers <- layers + 1
}

if(li_ma60){
  g <- g + geom_line(data = dt,color="#4169E1",size=trendlineWidth,aes(x=id,y=MA60))
  layers <- layers + 1
}

if(li_ma_c){
  g <- g + geom_line(data = dt,color=LineColor_1,size=trendlineWidth,aes(x=id,y=MA_C))
  layers <- layers + 1
}


g <- g + theme(
  axis.ticks=element_blank(),
  panel.background = element_rect(fill = "transparent"), 
  plot.background = element_rect(fill = "transparent"), 
  panel.grid.major = element_blank(), 
  legend.position = 'none',
  legend.background = element_rect(fill = "transparent"), 
  legend.box.background = element_rect(fill = "transparent")
) + labs(x = NULL,y=NULL)
############# Create and save widget ###############
p <- plotly_build(g)
if(show_Hover){
    if(show_Date){
        p$x$data[[1]]$text <-  paste(
        da_names, ": ", format(da,Format), "<br>", 
        cl_names, ": ", round(cl,2) ,"<br>", 
        op_names, ": ", round(op,2) ,"<br>", 
        hi_names, ": ", round(hi,2) ,"<br>", 
        lo_names, ": ", round(lo,2) ,"<br>", 
        sep ="" )
    } else {
        p$x$data[[1]]$text <-  paste(
        cl_names, ": ", round(cl,2) ,"<br>", 
        op_names, ": ", round(op,2) ,"<br>", 
        hi_names, ": ", round(hi,2) ,"<br>", 
        lo_names, ": ", round(lo,2) ,"<br>", 
        sep ="" )
    }

if(show_MA){
    if(li_ma_i<5){
        if(li_ma_c){
            p$x$data[[1]]$text <- paste(
            p$x$data[[1]]$text,ma_custom_name, ": ", round(dt$MA_C,2) ,"<br>", 
            sep ="" )
    }
    }

    if(li_ma05){
        p$x$data[[1]]$text <- paste(
        p$x$data[[1]]$text, "MA05", ": ", round(dt$MA05,2) ,"<br>", 
        sep ="" )
    }

    if(li_ma_i>=5 && li_ma_i<10){
        if(li_ma_c){
            p$x$data[[1]]$text <- paste(
            p$x$data[[1]]$text, ma_custom_name, ": ", round(dt$MA_C,2) ,"<br>", 
            sep ="" )
    }
    }

    if(li_ma10){
        p$x$data[[1]]$text <- paste(
        p$x$data[[1]]$text, "MA10", ": ", round(dt$MA10,2) ,"<br>", 
        sep ="" )
    }

    if(li_ma_i>=10 && li_ma_i<20){
        if(li_ma_c){
            p$x$data[[1]]$text <- paste(
            p$x$data[[1]]$text, ma_custom_name, ": ", round(dt$MA_C,2) ,"<br>", 
            sep ="" )
    }
    }

    if(li_ma20){
        p$x$data[[1]]$text <- paste(
        p$x$data[[1]]$text, "MA20", ": ", round(dt$MA20,2) ,"<br>", 
        sep ="" )
    }

    if(li_ma_i>=20 && li_ma_i<30){
        if(li_ma_c){
            p$x$data[[1]]$text <- paste(
            p$x$data[[1]]$text,ma_custom_name, ": ", round(dt$MA_C,2) ,"<br>", 
            sep ="" )
    }
    }

    if(li_ma30){
        p$x$data[[1]]$text <- paste(
        p$x$data[[1]]$text, "MA30", ": ", round(dt$MA30,2) ,"<br>", 
        sep ="" )
    }

    if(li_ma_i>=30 && li_ma_i<60){
        if(li_ma_c){
            p$x$data[[1]]$text <- paste(
            p$x$data[[1]]$text, ma_custom_name, ": ", round(dt$MA_C,2) ,"<br>", 
            sep ="" )
    }
    }

    if(li_ma60){
        p$x$data[[1]]$text <- paste(
        p$x$data[[1]]$text, "MA60", ": ", round(dt$MA60,2) ,"<br>", 
        sep ="" )
    }

    if(li_ma_i>=60){
        if(li_ma_c){
            p$x$data[[1]]$text <- paste(
            p$x$data[[1]]$text, ma_custom_name, ": ", round(dt$MA_C,2) ,"<br>", 
            sep ="" )
    }
    }
  }
} else {
  p$x$data[[1]]$hoverinfo <- "none"  
  #p$x$data[[3]]$hoverinfo <- "none"  
}

#p$x$data[[1]]$hoverinfo <- "none"
#p$x$data[[2]]$hoverinfo <- "none"


font <- list(
  family = h_f,
  size = h_f_Size,
  color = h_f_Color
)

hover_label <- list(
  bgcolor = h_Bg_Color,
  bordercolor = h_Border_Color,
  font = font,
  hoverinfo = "none"
)


p$x$data[[1]]$hoverlabel <- hover_label

modebarSetting = 'hover'
if(display_bar=='T'){
   modebarSetting<-TRUE
} else if(display_bar=='F'){
    modebarSetting<-FALSE
}


p$x$layout$spikedistance=1000
p$x$layout$hovermode='y unified'

p$x$layout$xaxis$rangemode = "tozero" ###
p$x$layout$xaxis$autorange = TRUE ###


p$x$layout$xaxis$spikethickness = 0.25
p$x$layout$xaxis$spikedash = "solid"
p$x$layout$xaxis$spikecolor = "blue"
p$x$layout$xaxis$spikemode = "across"


p$x$layout$yaxis$spikethickness = 0.25
p$x$layout$yaxis$spikedash = "solid"
p$x$layout$yaxis$spikecolor = "blue"
p$x$layout$yaxis$spikemode = "toaxis"
p$x$layout$yaxis$spikesnap = "cursor"

p$x$layout$yxaxis$autorange = TRUE
p$x$layout$yaxis$autotick = TRUE
p$x$layout$yaxis$showgrid = TRUE
p$x$layout$yaxis$tickmode = "auto" #This is the key for auto axis index.
p$x$layout$yaxis$separatethousands = TRUE

ay <- list(
  overlaying = "y",
  side = "left",
  size = 5,
  tickwidth = p$x$layout$yaxis$tickwidth
)



if(show_latest_cl){
    latest_cl_style <- "solid"
    if(exists("advance_latest_cl_style")){
        latest_cl_style <- advance_latest_cl_style
    }

    latest_cl_color <- "steelblue"
    if(exists("advance_latest_cl_color")){
    latest_cl_color <- advance_latest_cl_color
    } 

    p <- p %>% add_lines(y = dt$cl[nrow(dt)], yaxis = "y2", line=list(color=latest_cl_color,width=0.5,dash=latest_cl_style))
    p <- p %>% layout(
        yaxis2 = ay
    )
    p$x$layout$yaxis$gridcolor <- yGridColor
    p$x$layout$yaxis$griddash <- latest_cl_style
    p$x$layout$yaxis$gridwidth <- 0.5
    p$x$layout$yaxis2$gridcolor <- "blue"
    p$x$layout$yaxis2$overlaying <- "y"
    p$x$layout$yaxis2$tickfont$size <- p$x$layout$yaxis$tickfont$size
    p$x$layout$yaxis2$tickfont$family <- p$x$layout$yaxis$tickfont$family
    p$x$layout$yaxis2$tickvals <- list(dt$cl[nrow(dt)])
    p$x$layout$yaxis2$range <- p$x$layout$yaxis$range
    p$x$layout$yaxis2$ticktext <- p$x$layout$yaxis2$tickvals
    p$x$layout$yaxis2$tickfont$color <- latest_cl_color
}

for(i in 2:layers){
   p$x$data[[i]]$hoverinfo <- "none" 
}



p <- p %>% config(displaylogo = FALSE, cloud=FALSE,displayModeBar = modebarSetting ,
    modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","autoScale2d","hoverClosestCartesian"))        


internalSaveWidget(p, 'out.html');
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":[0-9]*,', ',"padding":0,')
####################################################
