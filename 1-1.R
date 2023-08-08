data.load <- function(file.path) {
  data.raw <- readLines(file.path) # 파일을 하나씩 읽기
  
  data.raw <- strsplit(data.raw, '\\|') # parsing
  
  data <- data.frame()
  data <- rbind(data, character(length(data.raw[[1]]))) # 파일 내용을 보관하는 dataframe을 만듦
  
  colnames(data) <- stringr::str_remove_all(data.raw[[1]], '\\"') # dataframe header 입력
  temp <- t(sapply(data.raw[2:length(data.raw)], function(x) {
    f.temp <- character(ncol(data))
    
    if(length(x) != 93) {
      f.temp <- c(x, '')
    } else {
      f.temp <- x
    }
    
    return(f.temp)
  }))
  colnames(temp) <- colnames(data)
  data <- rbind(data, temp) # dataframe에 파일 내용 입력
  
  data <- data[2:dim(data)[1], ] # 빈 row을 제거
  
  return(data)
}