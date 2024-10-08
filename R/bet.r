#' getThreshold
#'
#' bet returns the observed sensitivity thresholds of the subjects (=0 if the subject found the right sample for all concentrations, 1 if he/she found the right sample for all concentrations but the lightest one, etc.
#'@param minConc a number indicating the minimal concentration (not tested, but used for threshold calculations). If NULL, calculated as the lowest concentration - the average of the mean difference between two consecutive concentrations
#' @param maxConc  a number indicating the maximal concentration (not tested, but used for threshold calculations).If NULL, calculated as the lowest concentration + the average of the mean difference two consecutive concentrations
#' @param df dataframe whose colnames are 'subject' 'log_concentration' and 'AFC'
#' @export
#' @examples
#' data(astringency)
#' bet(astringency)
#' 
bet=function(df,minConc=0,maxConc=NULL)
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
  
  observedThreshold=rep(NA,length(subjects));names(observedThreshold)=subjects
 
  for(suj in subjects)
  {
    dataSuj=df[df[,"subject"]==suj,]
    rownames(dataSuj)=as.character(round(dataSuj[,"log_concentration"],digits=digits))
    dataSujOrdered=dataSuj[decreasingConcentrations,]
    # dataSujOrdered[,"avg"]=NA
    # dataSujOrdered[length(decreasingConcentrations),"avg"]=dataSujOrdered[length(decreasingConcentrations),"score"]
    # dataSujOrdered[-length(decreasingConcentrations),"avg"]=(dataSujOrdered[-length(decreasingConcentrations),"score"]+dataSujOrdered[-1,"score"])/2
    # 
    threshold=0
    i=1 # concentration index
    continue=TRUE
    while(i<length(decreasingConcentrations)+1&continue)
    {
      if(dataSujOrdered[decreasingConcentrations[i],"AFC"]==1)
      {
       # print("Succeed")
        i=i+1
      }
      else
      {
        if(i>1)
        {
          infoToRemember=dataSujOrdered[decreasingConcentrations[i-1],]
        }
        if(i==1)
        {
          infoToRemember=dataSujOrdered[decreasingConcentrations[i],]
        }
  #      print(infoToRemember)
        continue=FALSE
      }
    }
    observedThreshold[suj]=i-1
  # Si le sujet est très bon
    if(observedThreshold[suj]==length(decreasingConcentrations))
    {
      infoToRemember=dataSujOrdered[decreasingConcentrations[length(decreasingConcentrations)],]
    }

    if(observedThreshold[suj]==0)
    {
      infoToRemember=dataSujOrdered[1,]
    }

    #dfLastSucceed=rbind(dfLastSucceed,infoToRemember)
  }

  threshold=length(decreasingConcentrations)-observedThreshold

  # Calculation of threshold num

  thresholdNum=rep(NA,length(threshold))
  for(i in 1:length(observedThreshold))
  {
     thresholdNum[i]=thresholdToConcentration(threshold[i],decreasingNumConcentrations=decreasingNumConcentrations,minConc=minConc,maxConc=maxConc)
  }
  names(thresholdNum)=subjects
  return(thresholdNum)
}
