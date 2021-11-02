group.files <- function(ImageFolder, rename = TRUE) {

  MyFiles <- list.files(ImageFolder)
  Files <- tibble::as_tibble(MyFiles)
  Files <- dplyr::rename(Files, File = value)
  temp <- tibble::as_tibble(stringr::str_split_fixed(MyFiles, "_", 2))
  temp2 <- tibble::as_tibble(stringr::str_split_fixed(temp$V1, "-", 2))
  temp2 <- dplyr::rename(temp2, Run = V1, Plate = V2)
  Data <- dplyr::select(temp, V2)
  Data <- dplyr::rename(Data, Time = V2)
  Data <- cbind(Data, temp2, Files)
  Data$Plate <- stringr::str_remove(Data$Plate, "-2021")
  Data$Time <- stringr::str_remove(Data$Time, "-0.jpg")
  Data <- splitstackshape::cSplit(Data, "Time", sep = "_", fixed = FALSE)
  Data <- dplyr::rename(Data, Month = Time_1, Day = Time_2, Hour = Time_3, Minute = Time_4, Second = Time_5)
  Data <- dplyr::select(Data, File, Run, Plate, Month, Day, Hour, Minute, Second)
  Data <- dplyr::mutate(Data, NewFile = stringr::str_replace(File, paste("^\\S*", Data$Plate, sep = ""), Data$Plate))

  UniquePlates <- distinct(Data, Plate) #Work out what wells you have photos for
  Wells <- UniquePlates$Plate #Turn this into a list

  if (rename == TRUE){
    RenamedFiles <- Data$NewFile
    print("This is going to take a little while, the files are being renamed right now")
    file.rename(file.path(ImageFolder, MyFiles), file.path(ImageFolder, RenamedFiles))
    MyFiles <- list.files(ImageFolder)
    Data <- Data[order(NewFile),]
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
  print("Yay! Folders have been made. Now Going to save the Image data as ImageData.csv")
  write.csv(Data, "ImageData.csv")
  print("Time to move the images. This is going to take some time!")
  filesstrings::file.move(file.path(ImageFolder, MyFiles), file.path(folder, Data$Plate))
  print("All done!")
}

group.files("Images/Tahlia starvation3", rename = T)
