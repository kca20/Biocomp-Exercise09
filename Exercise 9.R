

#stddev(dir)
#On the premise that sep=" ". Might have to change it otherwise.


stddev<-function(dir){
      files<-list.files(dir, pattern=NULL, full.names = TRUE)
     for(x in files){
            data<-read.csv(x, sep = " ", header=FALSE) #reads all files in directory
            columnnumber<-readline(prompt = "Choose a column number:") #prompts user to choose column number.
            if (NROW(data[,as.numeric(columnnumber)])>=50){
                 vector<-c(vector,sd(data[,as.numeric(columnnumber)])/mean(data[,as.numeric(columnnumber)])) # if column number = or is > 50 find coefficient of variation and put in vector
              }else if(NROW(data[,as.numeric(columnnumber)])<50){  # if column number < 50 create error message and prompt user to verify action
                    verify<-readline(prompt = "ERROR:50 observations are not present in file.Overide?:")
                    if(verify=="yes"){
                         vector<-c(vector,sd(data[,as.numeric(columnnumber)])/mean(data[,as.numeric(columnnumber)])) #if user answers yes find coefficient of variation and put into vector
                     }
               }
           
         }
      return(vector)
}


