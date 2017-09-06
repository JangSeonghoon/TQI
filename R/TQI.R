#' TQI Function
#' 
#' This is for calculate TQI 
#' @pram 
#' @keywords 
#' @export

TQI=function(){
  
  fileList=list.files()
  print(fileList)
  
  startF=readline(prompt="몇번째 파일? :")
  startF=as.numeric(startF)
  lastF=startF #file parameter
  lastF=as.numeric(lastF)
  
  fileList=fileList[startF:lastF]
  
  
  ##위치 지정시에 TQI 산정을 위해서 원하는 범위를 200m로 나누어 떨어지게 잡아야 한다.
  ##ex) 19000 <= x < 19400
  
  print("1. m단위 입력")
  print("2. 범위를 200m로 구분되게 설정 ex)19000~19400")
  
  startD=readline(prompt="시작점: ") #distance parameter
  startD=as.numeric(startD)
  startD <<- startD
  lastD=readline(prompt="시작점: ") #distance parameter
  lastD=as.numeric(lastD)
  lastD<<-lastD
  
  ##startD는 원하는 범위 시작점-25(m기준)
  ##lastD는 원하는 범위 마지막점+24.75
  ##ex) 19000~19400 -> 18975 ~ 19424.75
  
  startD_50=startD-25 #distance parameter
  startD_50=as.numeric(startD_50)
  
  lastD_50=lastD+24.75 #distance parameter
  lastD_50=as.numeric(lastD_50)
  
  ##startD는 원하는 범위 시작점-100(m기준)
  ##lastD는 원하는 범위 마지막점+99.75
  ##ex) 19000~19400 -> 18900 ~ 19499.75
  
  startD_100=startD-100 #distance parameter
  startD_100=as.numeric(startD_100)
  
  lastD_100=lastD+99.75 #distance parameter
  lastD_100=as.numeric(lastD_100)
  
  
  range_no=(startD-1000)+0.25*(c(1:(4*lastD-4*startD+8001))-1)
  range_no<<-range_no
  
  ###############################################################################################
  ##이동평균선 고려한 rawData(50m)##
  ##############################
  
  longLevel=readline(prompt="이동평균 기준(50m/200m_단위 제외):") #평균수준 parameter(50 or 200)
  
  i=1
  coltypes=map.coltypes(fileList[i],header=T)
  temp=csvread(fileList[i],coltypes=coltypes, header=T)
  names(temp)[1]="distance"
  
  print(names(temp))
  
  kind=readline(prompt="틀림종류 : ") #틀림종류 parameter
  
  if(longLevel==50){
    temp=temp %>% select(1,4) %>% filter(distance>=startD_50,distance<=lastD_50)
    if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0
    movingInclude=rep(0,100)
    
    i=101;for(i in 101:(length(temp[,1])-100)){
      movingInclude[i]=temp[i,2]-mean(temp[c((i-100):(i-1),i,(i+1):(i+100)),2])
      print(paste0(i,"/",length(temp[,1])-100))
    }#for(i)
    movingInclude=c(movingInclude,rep(0,100))
    temp=mutate(temp,movingInclude=movingInclude)
    
  }else{
    temp=temp %>% select(1,4) %>% filter(distance>=startD_100,distance<=lastD_100)
    if(is.na(sum(temp[,2]))) temp[is.na(temp[,2]),2]=0
    movingInclude=rep(0,400)
    i=401;for(i in 401:(length(temp[,1])-400)){
      movingInclude[i]=temp[i,2]-mean(temp[c((i-400):(i-1),i,(i+1):(i+400)),2])
      print(paste0(i,"/",length(temp[,1])-400))
    }#for(i)
    movingInclude=c(movingInclude,rep(0,400))
    temp=mutate(temp,movingInclude=movingInclude)
  }#if
  
  
  ###############################################################################################
  ##TQI(50m)##
  ############
  
  #1. rawData TQI 산출
  
  range_TQI=(startD)+200*(c(1:((lastD-startD+200)/200))-1)
  range_TQI<<-range_TQI
  
  TQI=c()
  i=1;for(i in 1:(length(range_TQI)-1)){
    
    TQI[i]= sd((temp %>% select(1,3) %>% filter(distance>=range_TQI[i],distance<range_TQI[i+1]))[,2])
    print(paste0(i,"/",(length(range_TQI)-1)))
  }
  
  original_TQI=mean(TQI)
  original_TQI <<- original_TQI
  
  temp=temp %>% mutate(rawTQI=rep(0,length(temp[,1])))
  
  table_TQI=data.frame(start=range_TQI[-length(range_TQI)],TQI=TQI)
  i=1;for(i in 1:length(table_TQI[,1])){
    
    TrueOrNot=((temp[,1]-table_TQI[i,1])>0)&((temp[,1]-table_TQI[i,1])<200)
    temp[TrueOrNot,4]=table_TQI[i,2]
    
    print(paste0(i,"/",length(table_TQI[,1])))
  }
  
  temp<<-temp
  longLevel<<-longLevel
  
}#TQIfunction
