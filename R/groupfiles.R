group.files <- function(ImageFolder, rename = TRUE, savefile = filename) {

  MyFiles <- list.files(ImageFolder)
  if (identical(MyFiles, character(0))){
    stop("Images not found, make sure you point to your folder with images")
  }
  Files <- as.data.frame(MyFiles)
  Files <- dplyr::rename(Files, File = MyFiles)
  temp <- as.data.frame(stringr::str_split_fixed(MyFiles, "_", 2))
  temp2 <- as.data.frame(stringr::str_split_fixed(temp$V1, "-", 2))
  temp2 <- dplyr::rename(temp2, Run = V1, Plate = V2)
  Data <- dplyr::select(temp, V2)
  Data <- dplyr::rename(Data, Time = V2)
  Data <- cbind(Data, temp2, Files)
  Data$Plate <- stringr::str_remove(Data$Plate, "-2021")
  Data$Time <- stringr::str_remove(Data$Time, "-0.jpg")
  Data <- splitstackshape::cSplit(Data, "Time", sep = "_", type.convert = F)
  Data <- dplyr::rename(Data, Month = Time_1, Day = Time_2, Hour = Time_3, Minute = Time_4, Second = Time_5)
  Data <- dplyr::select(Data, File, Run, Plate, Month, Day, Hour, Minute, Second)
  Data <- dplyr::mutate(Data, NewFile = stringr::str_replace(File, paste("^\\S*", Data$Plate, sep = ""), Data$Plate))
  UniquePlates <- dplyr::distinct(Data, Plate)
  Wells <- UniquePlates$Plate
  if (rename == TRUE){
    RenamedFiles <- Data$NewFile
    print("This is going to take a little while, the files are being renamed right now")
    file.rename(file.path(ImageFolder, MyFiles), file.path(ImageFolder, RenamedFiles))
    MyFiles <- list.files(ImageFolder)
    Data <- Data[order(Data$NewFile),]
    print("Okay, files have finished renaming! Now time to make folders for them.")
  }
  wd <- getwd()
  folder <- "GroupedImages"
  for (i in 1:length(Wells)) {
    newfolder <- Wells[i]
    if (!dir.exists(file.path(wd, folder, newfolder))){
      dir.create(file.path(wd, folder, newfolder), recursive = TRUE)
    }
  }
  print("Yay! Folders have been made. Now Going to save the Image data as DataInfo[Your folder].csv")
  write.csv(Data, paste0("DataInfo", stringr::str_replace(ImageFolder, paste("^\\S*", "/", sep = ""), ""), ".csv"))
  print("Time to move the images. This is going to take some time!")
  filesstrings::file.move(file.path(ImageFolder, MyFiles), file.path(folder, Data$Plate))
  print("All done!")
}
