RemoveRows<-function(DataFrame, Columns, ColumnNames,Values){
        NumObs<-c(nrow(DataFrame))
        ColName<-c("Original Data Set")
        for (i in 1: length(Columns)){
                for (v in 1: length(Values)){
                        DataFrame[DataFrame[Columns[i]]==Values[v],]<-NA
                        DataFrame<-na.omit(DataFrame)
                }
                NumObs[i+1]<-nrow(DataFrame)
                ColName[i+1]<-ColumnNames[i]
        }
        NewFrame<-DataFrame
        return(list(Data.RmRows=data.frame(NewFrame),NumRows=data.frame(ColName, NumObs)))       
}


RecodeRank<-function(DataFrame, SplittingVariable, Variable,ReplacementValue=16,RemoveRows=FALSE){
                SumByCol<- sapply(split(x=DataFrame[,Variable],f=DataFrame[,SplittingVariable]),sum)
                RPList<-as.integer(names(SumByCol[SumByCol==0]))
                CodeAsNA<-DataFrame[,SplittingVariable] %in% RPList
                #print(DataFrame[CodeAsNA,])
                #print(DataFrame[!CodeAsNA,])
                DataFrame[,Variable]<--(DataFrame[,Variable])
                #print(DataFrame)
                if (RemoveRows==TRUE){
                        DataFrame<-DataFrame[!CodeAsNA,]
                        DataFrame[DataFrame[,Variable]==0,Variable]<--ReplacementValue
                }
                else if (RemoveRows==FALSE){
                        DataFrame[CodeAsNA,Variable]<-NA
                        #print(DataFrame)
                        DataFrame[(!CodeAsNA & (DataFrame[,Variable]==0)),Variable]<--ReplacementValue
                        #print(DataFrame)
                }
        NewFrame<-DataFrame                
        return(NewFrame)        
}

VariableDescription<-function(DataFrame){
        VariableType<-c()
        VariableName<-colnames(DataFrame)
        for (v in 1:length(DataFrame)){
                VariableType[v]<-class(DataFrame[,v])
        }
        Description<-data.frame(VariableName,VariableType)
        return (Description)
}

RecodePickByColumn<-function(DataFrame, SplittingVariable, Variables){
        for (v in 1:length(Variables)){
                SumByCol<- sapply(split(x=DataFrame[,Variables[v]],f=DataFrame[,SplittingVariable]),sum)
                RPList<-as.integer(names(SumByCol[SumByCol==0]))
                CodeAsNA<-DataFrame[,SplittingVariable] %in% RPList
                DataFrame[CodeAsNA,Variables[v]]<-NA
        }
        NewFrame<-DataFrame
        return(data.frame(NewFrame))        
}

RecodePickByGroups<-function(DataFrame,SplittingVariable,VariableGroups){
        for (i in 1:length(VariableGroups)){
                SumByGroup<-sapply(split(x=DataFrame[,VariableGroups[[i]]],f=DataFrame[,SplittingVariable]),sum)
                RPList<- as.integer(names(SumByGroup[SumByGroup==0]))
                CodeAsNA<-DataFrame[,SplittingVariable] %in% RPList
                DataFrame[CodeAsNA,VariableGroups[[i]]]<-NA
        }
        
        NewFrame<-DataFrame
        return(data.frame(NewFrame))
}

RecodePickByMixed<-function(DataFrame, SplittingVariable, VariableColumns, VariableGroups){
        for (v in 1:length(VariableColumns)){
                SumByCol<- sapply(split(x=DataFrame[,VariableColumns[v]],f=DataFrame[,SplittingVariable]),sum)
                RPList<-as.integer(names(SumByCol[SumByCol==0]))
                CodeAsNA<-DataFrame[,SplittingVariable] %in% RPList
                DataFrame[CodeAsNA,VariableColumns[v]]<-NA
        }
        for (i in 1:length(VariableGroups)){
                SumByGroup<-sapply(split(x=DataFrame[,VariableGroups[[i]]],f=DataFrame[,SplittingVariable]),sum)
                RPList<- as.integer(names(SumByGroup[SumByGroup==0]))
                CodeAsNA<-DataFrame[,SplittingVariable] %in% RPList
                DataFrame[CodeAsNA,VariableGroups[[i]]]<-NA
        }
        NewFrame<-DataFrame
        return(NewFrame)
}


ComputeCorrelation<-function(DataFrame,DependentVariable,Question,Order=0,Observations="complete.obs"){
        Correlations<-c()
        DataFrame[,DependentVariable]<- DataFrame[,DependentVariable]
        for (i in 1:length(Question)){
                Correlations[i]<-cor(DataFrame[,DependentVariable],DataFrame[,Question[i],],use=Observations)
        }
        CorrelationFrame<-data.frame(Question,Correlations)
        if (Order==-1){
                return (CorrelationFrame[order(-Correlations),])}
        else if (Order==1) {
                return (CorrelationFrame[order(Correlations),])}
        else {return(CorrelationFrame[order(Question),])}
        
}

CreateComponent<-function(DataFrame,Columns,ColumnWeights){
        Output<-list(WeightedData="",AdjustedData="")
        NewColumn<-c()
        print(length(Columns))
        for (i in 1 : length(Columns)){
                DataFrame[,Columns[i]]<-DataFrame[,Columns[i]]*ColumnWeights[i]
                #print(ColumnWeights[i])
        }
        Output$WeightedData<-as.data.frame(DataFrame)
        #print(nrow(DataFrame))
        #print(length(Columns))
        for(j in 1:nrow(DataFrame)) {
                SUM<-0
                DENOM<-0
                for (i in 1: length(Columns)){
                        if (!is.na(DataFrame[j,i])){
                                SUM<-SUM+DataFrame[j,i]
                                DENOM<-DENOM+ColumnWeights[i]
                                
                                #print(SUM)
                                #print(DENOM)
                        }
                        
                        else {next}                
                }
        
        if (DENOM==0){
                NewColumn[j]<-NA
        }
        else {NewColumn[j]<-(SUM/DENOM)}
        
        #print(NewColumn[j])
        }
        Output$AdjustedData<-NewColumn
        return(Output)
}
