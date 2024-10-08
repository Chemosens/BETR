#' analyseSores
#'
#' Returns the plots with the scores according to their concentrations
#' @inheritParams bet
#' @param subject if NULL (default), a graph of all subjects is produced. It could also contain the name of only one ore more subjects. 
#' @param y_add when representationAFC=='label', the label is displayed on the ordinate of the intensity + y_add
#' @importFrom ggplot2 ylim theme element_text xlab ylab element_line scale_x_continuous scale_y_continuous labs geom_text geom_abline scale_shape_manual geom_line theme_bw ggtitle aes ggplot geom_point scale_color_manual
#' @description This function returns graphical results for threshold and intensity with concentrations as abscissa and intensities as ordinates. This is done by subject. In this case, the IBT and BET can be calculated and represented. 
#' @export
#' @importFrom grDevices rainbow
#' @param ratio_ylim default to 10. If y_add is NULL, y_add=(y_max-y_min)/ratio_ylim
#' @examples
#' data(astringency)
#' graph(astringency,subject="S002")
graph=function(df,subject=NULL,
                     minConc=NULL,maxConc=NULL,y_add=NULL,ratio_ylim=10)
{
  AFC=log_concentration=intensity=NULL
  logConc=unique(df[,"log_concentration"])
  decreasingNumConcentrations=logConc[order(logConc, decreasing=T)]
  digits=2
  continue=T
  step=abs(mean(diff(decreasingNumConcentrations)))
  if(is.null(minConc)){minConc=decreasingNumConcentrations[length(decreasingNumConcentrations)]-step}
  if(is.null(maxConc)){maxConc=decreasingNumConcentrations[1]+step}
  while(continue)
  {
    if(any(duplicated(round(decreasingNumConcentrations,digits=digits)))){digits=digits+1}else{continue=FALSE}
    if(digits>14){stop("Some concentrations are not different enough")}   
  }
  decreasingConcentrations=as.character(round(decreasingNumConcentrations,digits=digits))
  J=length(decreasingConcentrations)
  
  
  # Initializations and default parameters
  score=concentration=concentration2=Res=thresholdNum=avg=NULL

  if(!is.null(subject))
  {
   df=df[df[,"subject"]%in%subject,]
  }
  if(is.null(subject)){subject=unique(df[,"subject"])}

   colors_values="black";names(colors_values)=subject
 


  # Initialisation of the graph concentration/ intensity
   p=ggplot(df,aes(x=log_concentration,y=intensity))+geom_line(color="black")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ggtitle("Scores according to log-concentrations") 

  # Getting intensity threshold
  ibt=NULL
  ibt=ibt(df, minConc=minConc,maxConc=maxConc)
  p=p+geom_point(x=ibt[1],y=0,shape=3,size=4,color="black")

  # Getting BET threshold
  bet=bet(df,minConc=minConc,maxConc=maxConc)
  p=p+geom_point(x=bet[1],y=0,size=4,shape=4,color="black")

  if(is.null(y_add)){y_add=(max(df[,"intensity"])-min(df[,"intensity"]))/ratio_ylim}
  df_wrong2=df
  df_wrong2[,"score"]=df[,"intensity"]+y_add
  p=p+geom_text(data=df_wrong2,mapping=aes(x=log_concentration,y=score,label=AFC),color="black")
  

  if(is.null(y_add)){y_add=0}
  ysup=10+y_add

  p=p+scale_y_continuous(breaks=0:10,limits=c(0,10+y_add))+
    ylab("intensity")+scale_x_continuous(breaks=rev(as.numeric(decreasingConcentrations)),limits=c(minConc,maxConc))+xlab("Log-concentration")
  p=p+theme(panel.grid.minor=element_line(color="white"))
  p=p+labs(caption="x for BET, + for IBT")
  p=p+theme(legend.position="none")+ggtitle(subject)
  listRes=list(p=p,ibt=ibt,bet=bet)
  return(listRes)
}