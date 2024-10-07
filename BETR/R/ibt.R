#' @title ibt
#' @description A threshold is calculated as the start of the highest slope (cumulative while no negative slope)
#' @param df a dataframe containing at least 3 columns: 'subject','log_concentration' and 'intensity'
#' @param minConc a number indicating the minimal concentration (not tested, but used for threshold calculations). If NULL, calculated as the lowest concentration - the average of the mean difference between two consecutive concentrations
#' @param maxConc  a number indicating the maximal concentration (not tested, but used for threshold calculations).If NULL, calculated as the lowest concentration + the average of the mean difference two consecutive concentrations
#' @export
#' @examples
#' data(astringency)
#' ibt(astringency)
ibt=function(df, minConc=NULL,maxConc=NULL)
{
  subjects=levels(factor(df[,"subject"]))
  threshold=rep(NA,length(subjects));names(threshold)=subjects
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
  for(subject in subjects)
  {
    intensityDatai=df[df[,"subject"]==subject,]
    scores=intensityDatai[,"intensity"]
    scores=as.numeric(scores)
    names(scores)=as.character(round(intensityDatai[,"log_concentration"],digits=digits))
    reorderedScores=rev(scores[decreasingConcentrations])
    difference=diff(reorderedScores)
    names(difference)=paste0(names(reorderedScores)[-1],"-",names(reorderedScores)[-length(reorderedScores)])
    difference2=difference
    difference2[difference2<=0]=NA
    vec=cumsum_right(difference2)
    ind=which.max(vec)[1]
    thresholdIndex=ind
    thresholdNum=thresholdToConcentration(thresholdIndex,decreasingNumConcentrations=decreasingNumConcentrations,minConc,maxConc)
    threshold[subject]=thresholdNum
  }
  return(threshold)
}