library(shiny)
mainfolder="../cfsdataInput/"
datafolder=file.path(mainfolder,"data")
all_parameters<-data.frame(mainfolder=NULL,subfolder=NULL,profile=NULL,profile_key=NULL,profile_value=NULL)
profiles=dir(path = datafolder,pattern="*.profile$",recursive=T)
for(profile_name in profiles){
	 path=file.path(datafolder,profile_name)
	 profile_data=read.table(path,sep="\t")
	 profile_type<-strsplit(profile_name,"/")[[1]]
	 all_parameters=rbind(all_parameters,data.frame(mainfolder=profile_type[1],subfolder=profile_type[2],profile=gsub(".profile", "", profile_type[3]),profile_key=profile_data$V1,profile_value=profile_data$V2))
}

sampleInfo<-read.table(file.path(mainfolder,"sampleInfo.tsv"),sep="\t",header=T)
row.names(sampleInfo)=sampleInfo$SampleID
randIDs<-read.table(file.path(mainfolder,"randomID.tsv"),sep="\t",header=T,stringsAsFactors=F)
sampleIDs=randIDs$SampleID
names(sampleIDs)=randIDs$rid
sampleRIDs=randIDs$rid
names(sampleRIDs)=randIDs$SampleID


swr = function(string, nwrap=20) {
    paste(strwrap(gsub("\\.+"," ",string), width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

