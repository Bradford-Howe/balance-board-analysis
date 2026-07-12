library(zoo)
library(dplyr)

# Save large data file.
setwd(LargeEnvirDir)

# saveRDS(AllTrials, file = "AllTrials.rds", compress = T)
# saveRDS(AllTrialsNew, file = "AllTrialsNew.rds", compress = T)


# Later, when you need it:
AllTrials = readRDS("AllTrials.rds")
AllTrialsNew = readRDS("AllTrialsNew.rds")

HomeDirectory = getwd()
setwd(HomeDirectory)

LargeEnvirDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data"
WiiReadDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/Original"
WiiWriteDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/ExtraCrispy(new)"
PlotDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/New Plots"

setwd(WiiReadDir)
setwd(WiiWriteDir)
setwd(PlotDir)

###############################
#### Read original data
#### The original data has no header. This loop pulls the data, gives it the
#### correct header per the Wii recording software documentation, and puts
#### it into a 2D list AllTrials[[Participant No.]][[Trial No.]]
###############################
setwd(WiiReadDir)

folders = list.files(".")

AllTrials = list()
TrialNames = list()

for (i in folders[1:length(folders)])
{
   setwd(paste0(WiiReadDir,"/",i,"/WII"))
   TrialNames[[i]] = list.files(".")
   for (j in TrialNames[[i]])
   {
      temp = read.table(j, header = F, sep = " ")
      colnames(temp) = c( "TimeOpen","Frc1","Frc2","Frc3",
                          "Frc4","copX","copY","FrcTot")
      AllTrials[[i]][[j]] = temp
   }
      rm(temp)
}

###############################
### Create edited data sets, put them into a new list, and writes them as a CSV
### to the appropriate folder.
###############################
AllTrialsNew = list()

Pnames = names(AllTrials)

### This loop looks in each trial to find all the duplicate recordings from a
### previous trial, retains the unique data, and places it into the AllTrialsNew
### list with the same name but appended with the letter "N" for new.
for (k in Pnames)
{
   #Check that each subsequent trial has unique data to be retained
   SumCheck = 0
   for (i in 2:length(AllTrials[[k]]))
   {
      a = AllTrials[[k]][[(i-1)]]
      b = AllTrials[[k]][[(i)]]
      # This should be true when the recording software continually appends the
      # trial files.
      if (nrow(a)<nrow(b))
      {
         # This double checks that what we identify as apended data from a 
         # previous trial truly is duplicated and not unique data to the current
         # trial being examined.
         SumCheck = SumCheck + sum(a!=b[1:nrow(a),])
      }
      print(c(i,SumCheck)) # Shows the current trial being examined and the sumcheck.
   }

   # Place checked data into new list with appended name.
   if (SumCheck == 0)
   {
      for (i in 1:(length(AllTrials[[k]])-1))
      {
         print(c(i,k))
         a = AllTrials[[k]][[(i)]]
         b = AllTrials[[k]][[(i+1)]]
         temp = b[(nrow(a)+1):nrow(b),]
         NewFileName = paste0(TrialNames[[k]][i+1],"N")
         AllTrialsNew[[k]][[NewFileName]]= temp
      }
   }
   rm(temp)
}

# This loop writes the edited data to CSV files from the AllTrialsNew list
for (k in Pnames)
{
   setwd(WiiWriteDir)
   NewDirName = paste0("P",k,"Data")
   if (!dir.exists(NewDirName))
   {
      dir.create(NewDirName)
   }
   setwd(paste0(WiiWriteDir,"/",NewDirName))
   
   TheseNamesNew = names(AllTrialsNew[[k]])
   
   for (i in TheseNamesNew)
   {
      print(c(i,k))
      temp = AllTrialsNew[[k]][[i]]
      write.table(temp,
                  paste0(i,".csv"),
                  sep = ",",
                  col.names = T,
                  row.names = F)
   }
}

########################
### Put plots in plot directory
########################
for (k in Pnames)
{
   TheseNamesNew = names(AllTrialsNew[[k]])
   
   # Some trials contained no unique data and so by eliminating the duplicated
   # data from the previous trial they have been made empty. This loop removes 
   # them from consideration for plot generation.
   for (i in TheseNamesNew)
   {
      a = AllTrialsNew[[k]][[i]]
      if (nrow(a) < 10 || sum(is.na(a)) > 100)
      {
         TheseNamesNew = TheseNamesNew[TheseNamesNew != i]
      }
   }
   
   setwd(PlotDir)
   NewDirName = paste0("P",k,"Plots")
   if (!dir.exists(NewDirName))
   {
      dir.create(NewDirName)
   }
   setwd(paste0(PlotDir,"/",NewDirName))
   
   for (i in TheseNamesNew)
   {
      NormalGraphicDevice = dev.cur() # Retaining default plot settings
      
      ThisTrialName = i
      
      CurrentData = AllTrialsNew[[k]][[ThisTrialName]]
      colnames(CurrentData) = c( "TimeOpen","Frc1","Frc2","Frc3",
                                 "Frc4","copX","copY","FrcTot")
      PlotFileName = paste0(i,".png")
         
      thisX = CurrentData$copX
      thisY = CurrentData$copY
      
      maxX = ceiling(length(thisX) / 500) * 500
      
      minY = min(c(thisX,thisY)) - 5
      maxY = max(c(thisX,thisY)) + 5
      
      png(PlotFileName,
          width = 800 + maxX/5,
          height = 1000 )
      
      # Widen margins to make room for y-axis label.
      par(mar = c(5, 6, 4, 2) + 0.1)
      
      plot(thisX,
           cex = 2,
           main = PlotFileName,
           cex.main = 3,
           sub = "Note: Horizontal lines are means",
           cex.sub = 3,
           xlab = '',
           ylab = "COP X & Y",
           cex.lab = 3,
           type = 'p',
           xlim = c(0,maxX),
           ylim = c(minY,maxY),
           cex.axis = 2,
           pch = 16,
           frame.plot = F,
           col = terrain.colors(9)
           )
      points(thisY,
             pch = 16,
             cex = 2,
             col = grey.colors(9))
      abline(h = mean(thisX),
             col = terrain.colors(9)[5],
             lw = 2)
      abline(h = mean(thisY),
             col = 'grey',
             lw = 2)
      legend('topright',
             c("COP X", "COP Y"),
             cex = 2,
             col = c(terrain.colors(9)[5],'grey'),
             pch = 16
             )
      
      # Close plotting device
      dev.off()
      # Restore default margins
      par(mar = c(5, 4, 4, 2) + 0.1)
      # Restore default graphic settings
      dev.set(NormalGraphicDevice)
   
   }
   # Double check that the file path is closed before looping to new participant.
   dev.off()
}


#######
### Spot checking
#######



TempX = AllTrials$`11`$p11t01$copX

length(TempX)


for (i in 1:length(AllTrials))
{
   a = nrow(AllTrials[[i]][[1]])
   
   #a = length(AllTrials[[i]])
   print(c(a,i))
}

for (i in 1:length(AllTrials))
{
   df = AllTrials[[i]][[1]]
   colnames(df) = c( "TimeOpen","Frc1","Frc2","Frc3",
                       "Frc4","copX","copY","FrcTot")
   df$TimeDif = c(0,df$TimeOpen[2:nrow(df)]-df$TimeOpen[1:(nrow(df)-1)])
   a = rownames(df[df$TimeDif>40,])%>% as.numeric()
   if (length(a)>0)
   {
      ThisName = paste0(names(AllTrials[[i]])[1],"N")
      f = df[c(a[length(a)]:nrow(df)),]
      RowCount = nrow(f)
      print(c(RowCount,i))
      AllTrialsNew[[i]][[ThisName]] = f
   } else 
   {
      ThisName = paste0(names(AllTrials[[i]])[1],"G")
      AllTrialsNew[[i]][[ThisName]] = df
      print(c(i,nrow(f),"good"))
   }
}

nrow(AllTrialsNew$`10`$p10t01N)


for (i in 1:length(AllTrialsNew))
{
   a = nrow(AllTrialsNew[[i]][[1]])
   
   #a = length(AllTrialsNew[[i]])
   print(c(a,i))
}


nrow(df)

plot(TempX,
     cex = 2,
     main = PlotFileName,
     cex.main = 3,
     sub = "Note: Horizontal lines are means",
     cex.sub = 3,
     xlab = '',
     ylab = "COP X & Y",
     cex.lab = 3,
     type = 'p',
     xlim = c(0,maxX),
     ylim = c(minY,maxY),
     cex.axis = 2,
     pch = 16,
     frame.plot = F,
     col = terrain.colors(9)
)
