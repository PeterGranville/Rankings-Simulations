
#### Setup #### 

library(dplyr)
library(readxl)
library(scales)
library(tidyverse)

#### End #### 

################################################
#### Set up datasets                        ####
################################################

#### Process data for national universities ####

nationalUniversities <- data.frame(
  `Name` = character(), 
  `Overall ranking` = numeric(),
  `Overall score` = numeric(), 
  `Outcomes rank` = numeric(),
  `Average 6-year graduation rate` = numeric(),
  `Average first-year student retention rate` = numeric(),
  `Social Mobility Rank` = numeric(),
  `6-year graduation rate of students who received a Pell Grant` = numeric(),
  `6-year graduation rate of students who did not receive a Pell Grant` = numeric(),
  `6-year graduation rate of first generation students` = numeric(),
  `6-year graduation rate of non-first generation students` = numeric(),
  `Predicted graduation rate` = numeric(),
  `Overperformance/Underperformance` = numeric(),
  `Median federal loan debt for borrowers` = numeric(),
  `College grads earning more than a HS grad` = numeric(),
  `Peer assessment score` = numeric(),
  `Faculty resources rank` = numeric(),
  `Faculty salaries` = numeric(),
  `Percent of faculty who are full-time` = numeric(),
  `Classes with fewer than 20 students` = numeric(),
  `Student-Faculty Ratio` = numeric(),
  `Financial resources rank` = numeric(),
  `SAT/ACT 25th-75th percentile` = numeric(),
  `Bibliometric Rank` = numeric(),
  
  `Contribution from Outcomes` = numeric(), 
  `Contribution from Average 6-year graduation rate` = numeric(), 
  `Contribution from Average first-year student retention rate` = numeric(),   
  `Contribution from 6-year graduation rate of students who received a Pell Grant` = numeric(),
  `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` = numeric(),
  `Contribution from 6-year graduation rate of first generation students` = numeric(),
  `Contribution from 6-year graduation rate of non-first generation students` = numeric(),
  `Contribution from Overperformance/Underperformance` = numeric(),
  `Contribution from Median federal loan debt for borrowers` = numeric(),
  `Contribution from College grads earning more than a HS grad` = numeric(),
  `Contribution from Expert Opinion` = numeric(),
  `Contribution from Faculty Resources` = numeric(),
  `Contribution from Faculty salaries` = numeric(),
  `Contribution from Percent of faculty who are full-time` = numeric(),
  `Contribution from Student-Faculty Ratio` = numeric(),
  `Contribution from Financial Resources` = numeric(),
  `Contribution from Student Excellence` = numeric(),
  `Contribution from Faculty Research` = numeric(),
  
  check.names=FALSE
)

for(i in (1:428)){
  
  print(paste("Trying number ", i, " of 428.", sep=""))
  
  universityName <- NA
  overallRanking <- NA
  overallScore <- NA
  outcomesRank <- NA
  gradRate6yr <- NA
  retentionRateFY <- NA
  mobilityRank <- NA
  gradRate6yrPell <- NA
  gradRate6yrNoPell <- NA
  gradRate6yrFirstGen <- NA
  gradRate6yrNotFirstGen <- NA
  predictedGradRate <- NA
  overunderPerform <- NA
  medianDebt <- NA
  thresholdHS <- NA
  peerScore <- NA
  facultyResourcesRank <- NA
  facultySalaries <- NA
  facultyFT <- NA
  smallClasses <- NA
  studentFacultyRatio <- NA
  resourcesRank <- NA
  standardizedTests <- NA
  bibliometricRank <- NA
  contribute.outcomesRank <- NA 
  contribute.gradRate6yr <- NA 
  contribute.retentionRateFY <- NA   
  contribute.gradRate6yrPell <- NA
  contribute.gradRate6yrNoPell <- NA
  contribute.gradRate6yrFirstGen <- NA
  contribute.gradRate6yrNotFirstGen <- NA
  contribute.overunderPerform <- NA
  contribute.medianDebt <- NA
  contribute.thresholdHS <- NA
  contribute.peerScore <- NA
  contribute.facultyResourcesRank <- NA
  contribute.facultySalaries <- NA
  contribute.facultyFT <- NA
  contribute.studentFacultyRatio <- NA
  contribute.resourcesRank <- NA
  contribute.standardizedTests <- NA
  contribute.bibliometricRank <- NA
  
  sheetName <- paste("Sheet", i, sep="")
  
  inputData <- read_excel("US News Data 2.xlsx", sheet=sheetName, col_names=FALSE) %>% rename(`V1` = `...1`)
  inputData$V1[is.na(inputData$V1)] <- ""
  
  for(j in (3:nrow(inputData))){
    if((inputData$V1[j-2]) %in% c("Add to List", "Added to List")){
      universityName <- inputData$V1[j]
    }
  }
  universityName <- gsub(" Rankings", "", universityName)
  rm(j)
  
  for(j in (1:nrow(inputData))){
    if((grepl("National Universities", inputData$V1[j])) & ((grepl("Schools are ranked", inputData$V1[j])==FALSE))){
      overallRanking <- inputData$V1[j]}
  }
  overallRanking <- ((strsplit(overallRanking, "out of"))[[1]])[1]
  suppressWarnings({
    overallRanking <- as.numeric(gsub("[^0-9.-]", "", overallRanking))
  })
  rm(j) 
  
  for(j in (3:nrow(inputData))){
    if((inputData$V1[j-2])=="Score (out of 100)"){
      suppressWarnings({
        overallScore <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if((inputData$V1[j-2])=="Outcomes rank"){
      suppressWarnings({
        outcomesRank <- as.numeric(inputData$V1[j])
      })
      contribute.outcomesRank0 <- inputData$V1[j-4]
      contribute.outcomesRank0 <- ((strsplit(contribute.outcomesRank0, " "))[[1]])
      for(k in (1:length(contribute.outcomesRank0))){
        if(grepl("%", contribute.outcomesRank0[k])){
          suppressWarnings({
            contribute.outcomesRank <- as.numeric(gsub("[^0-9.-]", "", contribute.outcomesRank0[k])) / 100
          })
        }
      }
      rm(contribute.outcomesRank0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Average 6-year graduation rate", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yr <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yr0 <- inputData$V1[j-2]
      contribute.gradRate6yr0 <- ((strsplit(contribute.gradRate6yr0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yr0))){
        if(grepl("%", contribute.gradRate6yr0[k])){
          suppressWarnings({
            contribute.gradRate6yr <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yr0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yr0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Average first-year student retention rate", inputData$V1[j-2])){
      suppressWarnings({
        retentionRateFY <- as.numeric(inputData$V1[j])
      })
      contribute.retentionRateFY0 <- inputData$V1[j-2]
      contribute.retentionRateFY0 <- ((strsplit(contribute.retentionRateFY0, " "))[[1]])
      for(k in (1:length(contribute.retentionRateFY0))){
        if(grepl("%", contribute.retentionRateFY0[k])){
          suppressWarnings({
            contribute.retentionRateFY <- as.numeric(gsub("[^0-9.-]", "", contribute.retentionRateFY0[k])) / 100
          })
        }
      }
      rm(contribute.retentionRateFY0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Social Mobility Rank", inputData$V1[j-2])){
      suppressWarnings({
        mobilityRank <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("6-year graduation rate of students who received a Pell Grant", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yrPell <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yrPell0 <- inputData$V1[j-2]
      contribute.gradRate6yrPell0 <- ((strsplit(contribute.gradRate6yrPell0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yrPell0))){
        if(grepl("%", contribute.gradRate6yrPell0[k])){
          suppressWarnings({
            contribute.gradRate6yrPell <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yrPell0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yrPell0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("6-year graduation rate of students who did not receive a Pell Grant", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yrNoPell <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yrNoPell0 <- inputData$V1[j-2]
      contribute.gradRate6yrNoPell0 <- ((strsplit(contribute.gradRate6yrNoPell0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yrNoPell0))){
        if(grepl("%", contribute.gradRate6yrNoPell0[k])){
          suppressWarnings({
            contribute.gradRate6yrNoPell <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yrNoPell0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yrNoPell0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("6-year graduation rate of first generation students", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yrFirstGen <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yrFirstGen0 <- inputData$V1[j-2]
      contribute.gradRate6yrFirstGen0 <- ((strsplit(contribute.gradRate6yrFirstGen0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yrFirstGen0))){
        if(grepl("%", contribute.gradRate6yrFirstGen0[k])){
          suppressWarnings({
            contribute.gradRate6yrFirstGen <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yrFirstGen0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yrFirstGen0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("6-year graduation rate of non-first generation students", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yrNotFirstGen <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yrNotFirstGen0 <- inputData$V1[j-2]
      contribute.gradRate6yrNotFirstGen0 <- ((strsplit(contribute.gradRate6yrNotFirstGen0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yrNotFirstGen0))){
        if(grepl("%", contribute.gradRate6yrNotFirstGen0[k])){
          suppressWarnings({
            contribute.gradRate6yrNotFirstGen <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yrNotFirstGen0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yrNotFirstGen0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Predicted graduation rate", inputData$V1[j-2])){
      suppressWarnings({
        predictedGradRate <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Overperformance", inputData$V1[j-2])){
      suppressWarnings({
        overunderPerform <- as.numeric(inputData$V1[j])
      })
      contribute.overunderPerform0 <- inputData$V1[j-2]
      contribute.overunderPerform0 <- ((strsplit(contribute.overunderPerform0, " "))[[1]])
      for(k in (1:length(contribute.overunderPerform0))){
        if(grepl("%", contribute.overunderPerform0[k])){
          suppressWarnings({
            contribute.overunderPerform <- as.numeric(gsub("[^0-9.-]", "", contribute.overunderPerform0[k])) / 100
          })
        }
      }
      rm(contribute.overunderPerform0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Median federal loan debt for borrowers", inputData$V1[j-2])){
      suppressWarnings({
        medianDebt <- as.numeric(inputData$V1[j])
      })
      contribute.medianDebt0 <- inputData$V1[j-2]
      contribute.medianDebt0 <- ((strsplit(contribute.medianDebt0, " "))[[1]])
      for(k in (1:length(contribute.medianDebt0))){
        if(grepl("%", contribute.medianDebt0[k])){
          suppressWarnings({
            contribute.medianDebt <- as.numeric(gsub("[^0-9.-]", "", contribute.medianDebt0[k])) / 100
          })
        }
      }
      rm(contribute.medianDebt0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("College grads earning more than a HS grad", inputData$V1[j-2])){
      suppressWarnings({
        thresholdHS <- as.numeric(inputData$V1[j])
      })
      contribute.thresholdHS0 <- inputData$V1[j-2]
      contribute.thresholdHS0 <- ((strsplit(contribute.thresholdHS0, " "))[[1]])
      for(k in (1:length(contribute.thresholdHS0))){
        if(grepl("%", contribute.thresholdHS0[k])){
          suppressWarnings({
            contribute.thresholdHS <- as.numeric(gsub("[^0-9.-]", "", contribute.thresholdHS0[k])) / 100
          })
        }
      }
      rm(contribute.thresholdHS0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Peer assessment score", inputData$V1[j-2])){
      suppressWarnings({
        peerScore <- as.numeric(inputData$V1[j])
      })
      contribute.peerScore0 <- inputData$V1[j-4]
      contribute.peerScore0 <- ((strsplit(contribute.peerScore0, " "))[[1]])
      for(k in (1:length(contribute.peerScore0))){
        if(grepl("%", contribute.peerScore0[k])){
          suppressWarnings({
            contribute.peerScore <- as.numeric(gsub("[^0-9.-]", "", contribute.peerScore0[k])) / 100
          })
        }
      }
      rm(contribute.peerScore0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Faculty resources rank", inputData$V1[j-2])){
      suppressWarnings({
        facultyResourcesRank <- as.numeric(inputData$V1[j])
      })
      contribute.facultyResourcesRank0 <- inputData$V1[j-4]
      contribute.facultyResourcesRank0 <- ((strsplit(contribute.facultyResourcesRank0, " "))[[1]])
      for(k in (1:length(contribute.facultyResourcesRank0))){
        if(grepl("%", contribute.facultyResourcesRank0[k])){
          suppressWarnings({
            contribute.facultyResourcesRank <- as.numeric(gsub("[^0-9.-]", "", contribute.facultyResourcesRank0[k])) / 100
          })
        }
      }
      rm(contribute.facultyResourcesRank0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Faculty salaries", inputData$V1[j-2])){
      suppressWarnings({
        facultySalaries <- as.numeric(inputData$V1[j])
      })
      contribute.facultySalaries0 <- inputData$V1[j-2]
      contribute.facultySalaries0 <- ((strsplit(contribute.facultySalaries0, " "))[[1]])
      for(k in (1:length(contribute.facultySalaries0))){
        if(grepl("%", contribute.facultySalaries0[k])){
          suppressWarnings({
            contribute.facultySalaries <- as.numeric(gsub("[^0-9.-]", "", contribute.facultySalaries0[k])) / 100
          })
        }
      }
      rm(contribute.facultySalaries0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Percent of faculty who are full-time", inputData$V1[j-2])){
      suppressWarnings({
        facultyFT <- as.numeric(inputData$V1[j])
      })
      contribute.facultyFT0 <- inputData$V1[j-2]
      contribute.facultyFT0 <- ((strsplit(contribute.facultyFT0, " "))[[1]])
      for(k in (1:length(contribute.facultyFT0))){
        if(grepl("%", contribute.facultyFT0[k])){
          suppressWarnings({
            contribute.facultyFT <- as.numeric(gsub("[^0-9.-]", "", contribute.facultyFT0[k])) / 100
          })
        }
      }
      rm(contribute.facultyFT0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Classes with fewer than 20 students", inputData$V1[j-2])){
      suppressWarnings({
        smallClasses <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Student-Faculty Ratio", inputData$V1[j-2])){
      suppressWarnings({
        studentFacultyRatio <- as.numeric(inputData$V1[j]) 
      })
      studentFacultyRatio <- round((floor(studentFacultyRatio * 1000) / 1000) * 24, digits=0)
      contribute.studentFacultyRatio0 <- inputData$V1[j-2]
      contribute.studentFacultyRatio0 <- ((strsplit(contribute.studentFacultyRatio0, " "))[[1]])
      for(k in (1:length(contribute.studentFacultyRatio0))){
        if(grepl("%", contribute.studentFacultyRatio0[k])){
          suppressWarnings({
            contribute.studentFacultyRatio <- as.numeric(gsub("[^0-9.-]", "", contribute.studentFacultyRatio0[k])) / 100
          })
        }
      }
      rm(contribute.studentFacultyRatio0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Financial resources rank", inputData$V1[j-2])){
      suppressWarnings({
        resourcesRank <- as.numeric(inputData$V1[j])
      })
      contribute.resourcesRank0 <- inputData$V1[j-4]
      contribute.resourcesRank0 <- ((strsplit(contribute.resourcesRank0, " "))[[1]])
      for(k in (1:length(contribute.resourcesRank0))){
        if(grepl("%", contribute.resourcesRank0[k])){
          suppressWarnings({
            contribute.resourcesRank <- as.numeric(gsub("[^0-9.-]", "", contribute.resourcesRank0[k])) / 100
          })
        }
      }
      rm(contribute.resourcesRank0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("25th-75th percentile", inputData$V1[j-2])){
      standardizedTests <- as.character(inputData$V1[j])
      suppressWarnings({
        standardizedTests <- mean(c(
          as.numeric(((strsplit(standardizedTests, "-"))[[1]])[1]), 
          as.numeric(((strsplit(standardizedTests, "-"))[[1]])[2])
        ))
      })
      if(is.na(standardizedTests)==FALSE){
        if(standardizedTests < 99){
          standardizedTests <- (((standardizedTests - 17)) * 34) + 935
        }
      }
      contribute.standardizedTests0 <- inputData$V1[j-4]
      contribute.standardizedTests0 <- ((strsplit(contribute.standardizedTests0, " "))[[1]])
      for(k in (1:length(contribute.standardizedTests0))){
        if(grepl("%", contribute.standardizedTests0[k])){
          suppressWarnings({
            contribute.standardizedTests <- as.numeric(gsub("[^0-9.-]", "", contribute.standardizedTests0[k])) / 100
          })
        }
      }
      rm(contribute.standardizedTests0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Bibliometric Rank", inputData$V1[j-2])){
      suppressWarnings({
        bibliometricRank <- as.numeric(inputData$V1[j])
      })
      contribute.bibliometricRank0 <- inputData$V1[j-4]
      contribute.bibliometricRank0 <- ((strsplit(contribute.bibliometricRank0, " "))[[1]])
      for(k in (1:length(contribute.bibliometricRank0))){
        if(grepl("%", contribute.bibliometricRank0[k])){
          suppressWarnings({
            contribute.bibliometricRank <- as.numeric(gsub("[^0-9.-]", "", contribute.bibliometricRank0[k])) / 100
          })
        }
      }
      rm(contribute.bibliometricRank0, k)
    }
  }
  rm(j)
  
  nationalUniversities <- nationalUniversities %>% add_row(
    `Name` = universityName, 
    `Overall ranking` = overallRanking,
    `Overall score` = overallScore, 
    `Outcomes rank` = outcomesRank,
    `Average 6-year graduation rate` = gradRate6yr,
    `Average first-year student retention rate` = retentionRateFY,
    `Social Mobility Rank` = mobilityRank,
    `6-year graduation rate of students who received a Pell Grant` = gradRate6yrPell,
    `6-year graduation rate of students who did not receive a Pell Grant` = gradRate6yrNoPell,
    `6-year graduation rate of first generation students` = gradRate6yrFirstGen,
    `6-year graduation rate of non-first generation students` = gradRate6yrNotFirstGen,
    `Predicted graduation rate` = predictedGradRate,
    `Overperformance/Underperformance` = overunderPerform,
    `Median federal loan debt for borrowers` = medianDebt,
    `College grads earning more than a HS grad` = thresholdHS,
    `Peer assessment score` = peerScore,
    `Faculty resources rank` = facultyResourcesRank,
    `Faculty salaries` = facultySalaries,
    `Percent of faculty who are full-time` = facultyFT,
    `Classes with fewer than 20 students` = smallClasses,
    `Student-Faculty Ratio` = studentFacultyRatio,
    `Financial resources rank` = resourcesRank,
    `SAT/ACT 25th-75th percentile` = standardizedTests,
    `Bibliometric Rank` = bibliometricRank,
    
    `Contribution from Outcomes` = contribute.outcomesRank, 
    `Contribution from Average 6-year graduation rate` = contribute.gradRate6yr, 
    `Contribution from Average first-year student retention rate` = contribute.retentionRateFY,   
    `Contribution from 6-year graduation rate of students who received a Pell Grant` = contribute.gradRate6yrPell,
    `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` = contribute.gradRate6yrNoPell,
    `Contribution from 6-year graduation rate of first generation students` = contribute.gradRate6yrFirstGen,
    `Contribution from 6-year graduation rate of non-first generation students` = contribute.gradRate6yrNotFirstGen,
    `Contribution from Overperformance/Underperformance` = contribute.overunderPerform,
    `Contribution from Median federal loan debt for borrowers` = contribute.medianDebt,
    `Contribution from College grads earning more than a HS grad` = contribute.thresholdHS,
    `Contribution from Expert Opinion` = contribute.peerScore,
    `Contribution from Faculty Resources` = contribute.facultyResourcesRank,
    `Contribution from Faculty salaries` = contribute.facultySalaries,
    `Contribution from Percent of faculty who are full-time` = contribute.facultyFT,
    `Contribution from Student-Faculty Ratio` = contribute.studentFacultyRatio,
    `Contribution from Financial Resources` = contribute.resourcesRank,
    `Contribution from Student Excellence` = contribute.standardizedTests,
    `Contribution from Faculty Research` = contribute.bibliometricRank,
  )
  
  rm(
    universityName,
    overallRanking,
    overallScore,
    outcomesRank,
    gradRate6yr,
    retentionRateFY,
    mobilityRank,
    gradRate6yrPell,
    gradRate6yrNoPell,
    gradRate6yrFirstGen,
    gradRate6yrNotFirstGen,
    predictedGradRate,
    overunderPerform,
    medianDebt,
    thresholdHS,
    peerScore,
    facultyResourcesRank,
    facultySalaries,
    facultyFT,
    smallClasses,
    studentFacultyRatio,
    resourcesRank,
    standardizedTests,
    bibliometricRank, 
    contribute.outcomesRank, 
    contribute.gradRate6yr, 
    contribute.retentionRateFY,   
    contribute.gradRate6yrPell,
    contribute.gradRate6yrNoPell,
    contribute.gradRate6yrFirstGen,
    contribute.gradRate6yrNotFirstGen,
    contribute.overunderPerform,
    contribute.medianDebt,
    contribute.thresholdHS,
    contribute.peerScore,
    contribute.facultyResourcesRank,
    contribute.facultySalaries,
    contribute.facultyFT,
    contribute.studentFacultyRatio,
    contribute.resourcesRank,
    contribute.standardizedTests,
    contribute.bibliometricRank, 
    inputData, 
    sheetName
  )
  
}
rm(i)

#### End #### 

#### Process data for national liberal arts colleges ####

nationalLAs <- data.frame(
  `Name` = character(), 
  `Overall ranking` = numeric(),
  `Overall score` = numeric(), 
  `Outcomes rank` = numeric(),
  `Average 6-year graduation rate` = numeric(),
  `Average first-year student retention rate` = numeric(),
  `Social Mobility Rank` = numeric(),
  `6-year graduation rate of students who received a Pell Grant` = numeric(),
  `6-year graduation rate of students who did not receive a Pell Grant` = numeric(),
  `Predicted graduation rate` = numeric(),
  `Overperformance/Underperformance` = numeric(),
  `Median federal loan debt for borrowers` = numeric(),
  `College grads earning more than a HS grad` = numeric(),
  `Peer assessment score` = numeric(),
  `Faculty resources rank` = numeric(),
  `Faculty salaries` = numeric(),
  `Percent of faculty who are full-time` = numeric(),
  `Classes with fewer than 20 students` = numeric(),
  `Student-Faculty Ratio` = numeric(),
  `Financial resources rank` = numeric(),
  `SAT/ACT 25th-75th percentile` = numeric(),

  `Contribution from Outcomes` = numeric(), 
  `Contribution from Average 6-year graduation rate` = numeric(), 
  `Contribution from Average first-year student retention rate` = numeric(),   
  `Contribution from 6-year graduation rate of students who received a Pell Grant` = numeric(),
  `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` = numeric(),
  `Contribution from Overperformance/Underperformance` = numeric(),
  `Contribution from Median federal loan debt for borrowers` = numeric(),
  `Contribution from College grads earning more than a HS grad` = numeric(),
  `Contribution from Expert Opinion` = numeric(),
  `Contribution from Faculty Resources` = numeric(),
  `Contribution from Faculty salaries` = numeric(),
  `Contribution from Percent of faculty who are full-time` = numeric(),
  `Contribution from Student-Faculty Ratio` = numeric(),
  `Contribution from Financial Resources` = numeric(),
  `Contribution from Student Excellence` = numeric(),

  check.names=FALSE
)

for(i in (1:204)){
  
  print(paste("Trying number ", i, " of 204.", sep=""))
  
  universityName <- NA
  overallRanking <- NA
  overallScore <- NA
  outcomesRank <- NA
  gradRate6yr <- NA
  retentionRateFY <- NA
  mobilityRank <- NA
  gradRate6yrPell <- NA
  gradRate6yrNoPell <- NA
  predictedGradRate <- NA
  overunderPerform <- NA
  medianDebt <- NA
  thresholdHS <- NA
  peerScore <- NA
  facultyResourcesRank <- NA
  facultySalaries <- NA
  facultyFT <- NA
  smallClasses <- NA
  studentFacultyRatio <- NA
  resourcesRank <- NA
  standardizedTests <- NA
  contribute.outcomesRank <- NA 
  contribute.gradRate6yr <- NA 
  contribute.retentionRateFY <- NA   
  contribute.gradRate6yrPell <- NA
  contribute.gradRate6yrNoPell <- NA
  contribute.overunderPerform <- NA
  contribute.medianDebt <- NA
  contribute.thresholdHS <- NA
  contribute.peerScore <- NA
  contribute.facultyResourcesRank <- NA
  contribute.facultySalaries <- NA
  contribute.facultyFT <- NA
  contribute.studentFacultyRatio <- NA
  contribute.resourcesRank <- NA
  contribute.standardizedTests <- NA

  sheetName <- paste("Sheet", i, sep="")
  
  inputData <- read_excel("US News Data 4.xlsx", sheet=sheetName, col_names=FALSE) %>% rename(`V1` = `...1`)
  inputData$V1[is.na(inputData$V1)] <- ""
  
  for(j in (3:nrow(inputData))){
    if((inputData$V1[j-2]) %in% c("Add to List", "Added to List")){
      universityName <- inputData$V1[j]
    }
  }
  universityName <- gsub(" Rankings", "", universityName)
  rm(j)
  
  for(j in (1:nrow(inputData))){
    if((grepl("National Liberal Arts Colleges", inputData$V1[j])) & ((grepl("Schools are ranked", inputData$V1[j])==FALSE))){
      overallRanking <- inputData$V1[j]}
  }
  overallRanking <- ((strsplit(overallRanking, "out of"))[[1]])[1]
  suppressWarnings({
    overallRanking <- as.numeric(gsub("[^0-9.-]", "", overallRanking))
  })
  rm(j) 
  
  for(j in (3:nrow(inputData))){
    if((inputData$V1[j-2])=="Score (out of 100)"){
      suppressWarnings({
        overallScore <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if((inputData$V1[j-2])=="Outcomes rank"){
      suppressWarnings({
        outcomesRank <- as.numeric(inputData$V1[j])
      })
      contribute.outcomesRank0 <- inputData$V1[j-4]
      contribute.outcomesRank0 <- ((strsplit(contribute.outcomesRank0, " "))[[1]])
      for(k in (1:length(contribute.outcomesRank0))){
        if(grepl("%", contribute.outcomesRank0[k])){
          suppressWarnings({
            contribute.outcomesRank <- as.numeric(gsub("[^0-9.-]", "", contribute.outcomesRank0[k])) / 100
          })
        }
      }
      rm(contribute.outcomesRank0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Average 6-year graduation rate", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yr <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yr0 <- inputData$V1[j-2]
      contribute.gradRate6yr0 <- ((strsplit(contribute.gradRate6yr0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yr0))){
        if(grepl("%", contribute.gradRate6yr0[k])){
          suppressWarnings({
            contribute.gradRate6yr <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yr0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yr0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Average first-year student retention rate", inputData$V1[j-2])){
      suppressWarnings({
        retentionRateFY <- as.numeric(inputData$V1[j])
      })
      contribute.retentionRateFY0 <- inputData$V1[j-2]
      contribute.retentionRateFY0 <- ((strsplit(contribute.retentionRateFY0, " "))[[1]])
      for(k in (1:length(contribute.retentionRateFY0))){
        if(grepl("%", contribute.retentionRateFY0[k])){
          suppressWarnings({
            contribute.retentionRateFY <- as.numeric(gsub("[^0-9.-]", "", contribute.retentionRateFY0[k])) / 100
          })
        }
      }
      rm(contribute.retentionRateFY0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Social Mobility Rank", inputData$V1[j-2])){
      suppressWarnings({
        mobilityRank <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("6-year graduation rate of students who received a Pell Grant", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yrPell <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yrPell0 <- inputData$V1[j-2]
      contribute.gradRate6yrPell0 <- ((strsplit(contribute.gradRate6yrPell0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yrPell0))){
        if(grepl("%", contribute.gradRate6yrPell0[k])){
          suppressWarnings({
            contribute.gradRate6yrPell <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yrPell0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yrPell0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("6-year graduation rate of students who did not receive a Pell Grant", inputData$V1[j-2])){
      suppressWarnings({
        gradRate6yrNoPell <- as.numeric(inputData$V1[j])
      })
      contribute.gradRate6yrNoPell0 <- inputData$V1[j-2]
      contribute.gradRate6yrNoPell0 <- ((strsplit(contribute.gradRate6yrNoPell0, " "))[[1]])
      for(k in (1:length(contribute.gradRate6yrNoPell0))){
        if(grepl("%", contribute.gradRate6yrNoPell0[k])){
          suppressWarnings({
            contribute.gradRate6yrNoPell <- as.numeric(gsub("[^0-9.-]", "", contribute.gradRate6yrNoPell0[k])) / 100
          })
        }
      }
      rm(contribute.gradRate6yrNoPell0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Predicted graduation rate", inputData$V1[j-2])){
      suppressWarnings({
        predictedGradRate <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Overperformance", inputData$V1[j-2])){
      suppressWarnings({
        overunderPerform <- as.numeric(inputData$V1[j])
      })
      contribute.overunderPerform0 <- inputData$V1[j-2]
      contribute.overunderPerform0 <- ((strsplit(contribute.overunderPerform0, " "))[[1]])
      for(k in (1:length(contribute.overunderPerform0))){
        if(grepl("%", contribute.overunderPerform0[k])){
          suppressWarnings({
            contribute.overunderPerform <- as.numeric(gsub("[^0-9.-]", "", contribute.overunderPerform0[k])) / 100
          })
        }
      }
      rm(contribute.overunderPerform0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Median federal loan debt for borrowers", inputData$V1[j-2])){
      suppressWarnings({
        medianDebt <- as.numeric(inputData$V1[j])
      })
      contribute.medianDebt0 <- inputData$V1[j-2]
      contribute.medianDebt0 <- ((strsplit(contribute.medianDebt0, " "))[[1]])
      for(k in (1:length(contribute.medianDebt0))){
        if(grepl("%", contribute.medianDebt0[k])){
          suppressWarnings({
            contribute.medianDebt <- as.numeric(gsub("[^0-9.-]", "", contribute.medianDebt0[k])) / 100
          })
        }
      }
      rm(contribute.medianDebt0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("College grads earning more than a HS grad", inputData$V1[j-2])){
      suppressWarnings({
        thresholdHS <- as.numeric(inputData$V1[j])
      })
      contribute.thresholdHS0 <- inputData$V1[j-2]
      contribute.thresholdHS0 <- ((strsplit(contribute.thresholdHS0, " "))[[1]])
      for(k in (1:length(contribute.thresholdHS0))){
        if(grepl("%", contribute.thresholdHS0[k])){
          suppressWarnings({
            contribute.thresholdHS <- as.numeric(gsub("[^0-9.-]", "", contribute.thresholdHS0[k])) / 100
          })
        }
      }
      rm(contribute.thresholdHS0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Peer assessment score", inputData$V1[j-2])){
      suppressWarnings({
        peerScore <- as.numeric(inputData$V1[j])
      })
      contribute.peerScore0 <- inputData$V1[j-4]
      contribute.peerScore0 <- ((strsplit(contribute.peerScore0, " "))[[1]])
      for(k in (1:length(contribute.peerScore0))){
        if(grepl("%", contribute.peerScore0[k])){
          suppressWarnings({
            contribute.peerScore <- as.numeric(gsub("[^0-9.-]", "", contribute.peerScore0[k])) / 100
          })
        }
      }
      rm(contribute.peerScore0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Faculty resources rank", inputData$V1[j-2])){
      suppressWarnings({
        facultyResourcesRank <- as.numeric(inputData$V1[j])
      })
      contribute.facultyResourcesRank0 <- inputData$V1[j-4]
      contribute.facultyResourcesRank0 <- ((strsplit(contribute.facultyResourcesRank0, " "))[[1]])
      for(k in (1:length(contribute.facultyResourcesRank0))){
        if(grepl("%", contribute.facultyResourcesRank0[k])){
          suppressWarnings({
            contribute.facultyResourcesRank <- as.numeric(gsub("[^0-9.-]", "", contribute.facultyResourcesRank0[k])) / 100
          })
        }
      }
      rm(contribute.facultyResourcesRank0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Faculty salaries", inputData$V1[j-2])){
      suppressWarnings({
        facultySalaries <- as.numeric(inputData$V1[j])
      })
      contribute.facultySalaries0 <- inputData$V1[j-2]
      contribute.facultySalaries0 <- ((strsplit(contribute.facultySalaries0, " "))[[1]])
      for(k in (1:length(contribute.facultySalaries0))){
        if(grepl("%", contribute.facultySalaries0[k])){
          suppressWarnings({
            contribute.facultySalaries <- as.numeric(gsub("[^0-9.-]", "", contribute.facultySalaries0[k])) / 100
          })
        }
      }
      rm(contribute.facultySalaries0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Percent of faculty who are full-time", inputData$V1[j-2])){
      suppressWarnings({
        facultyFT <- as.numeric(inputData$V1[j])
      })
      contribute.facultyFT0 <- inputData$V1[j-2]
      contribute.facultyFT0 <- ((strsplit(contribute.facultyFT0, " "))[[1]])
      for(k in (1:length(contribute.facultyFT0))){
        if(grepl("%", contribute.facultyFT0[k])){
          suppressWarnings({
            contribute.facultyFT <- as.numeric(gsub("[^0-9.-]", "", contribute.facultyFT0[k])) / 100
          })
        }
      }
      rm(contribute.facultyFT0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Classes with fewer than 20 students", inputData$V1[j-2])){
      suppressWarnings({
        smallClasses <- as.numeric(inputData$V1[j])
      })
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Student-Faculty Ratio", inputData$V1[j-2])){
      suppressWarnings({
        studentFacultyRatio <- as.numeric(inputData$V1[j]) 
      })
      studentFacultyRatio <- round((floor(studentFacultyRatio * 1000) / 1000) * 24, digits=0)
      contribute.studentFacultyRatio0 <- inputData$V1[j-2]
      contribute.studentFacultyRatio0 <- ((strsplit(contribute.studentFacultyRatio0, " "))[[1]])
      for(k in (1:length(contribute.studentFacultyRatio0))){
        if(grepl("%", contribute.studentFacultyRatio0[k])){
          suppressWarnings({
            contribute.studentFacultyRatio <- as.numeric(gsub("[^0-9.-]", "", contribute.studentFacultyRatio0[k])) / 100
          })
        }
      }
      rm(contribute.studentFacultyRatio0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("Financial resources rank", inputData$V1[j-2])){
      suppressWarnings({
        resourcesRank <- as.numeric(inputData$V1[j])
      })
      contribute.resourcesRank0 <- inputData$V1[j-4]
      contribute.resourcesRank0 <- ((strsplit(contribute.resourcesRank0, " "))[[1]])
      for(k in (1:length(contribute.resourcesRank0))){
        if(grepl("%", contribute.resourcesRank0[k])){
          suppressWarnings({
            contribute.resourcesRank <- as.numeric(gsub("[^0-9.-]", "", contribute.resourcesRank0[k])) / 100
          })
        }
      }
      rm(contribute.resourcesRank0, k)
    }
  }
  rm(j)
  
  for(j in (3:nrow(inputData))){
    if(grepl("25th-75th percentile", inputData$V1[j-2])){
      standardizedTests <- as.character(inputData$V1[j])
      suppressWarnings({
        standardizedTests <- mean(c(
          as.numeric(((strsplit(standardizedTests, "-"))[[1]])[1]), 
          as.numeric(((strsplit(standardizedTests, "-"))[[1]])[2])
        ))
      })
      if(is.na(standardizedTests)==FALSE){
        if(standardizedTests < 99){
          standardizedTests <- (((standardizedTests - 17)) * 34) + 935
        }
      }
      contribute.standardizedTests0 <- inputData$V1[j-4]
      contribute.standardizedTests0 <- ((strsplit(contribute.standardizedTests0, " "))[[1]])
      for(k in (1:length(contribute.standardizedTests0))){
        if(grepl("%", contribute.standardizedTests0[k])){
          suppressWarnings({
            contribute.standardizedTests <- as.numeric(gsub("[^0-9.-]", "", contribute.standardizedTests0[k])) / 100
          })
        }
      }
      rm(contribute.standardizedTests0, k)
    }
  }
  rm(j)
  
  nationalLAs <- nationalLAs %>% add_row(
    `Name` = universityName, 
    `Overall ranking` = overallRanking,
    `Overall score` = overallScore, 
    `Outcomes rank` = outcomesRank,
    `Average 6-year graduation rate` = gradRate6yr,
    `Average first-year student retention rate` = retentionRateFY,
    `Social Mobility Rank` = mobilityRank,
    `6-year graduation rate of students who received a Pell Grant` = gradRate6yrPell,
    `6-year graduation rate of students who did not receive a Pell Grant` = gradRate6yrNoPell,
    `Predicted graduation rate` = predictedGradRate,
    `Overperformance/Underperformance` = overunderPerform,
    `Median federal loan debt for borrowers` = medianDebt,
    `College grads earning more than a HS grad` = thresholdHS,
    `Peer assessment score` = peerScore,
    `Faculty resources rank` = facultyResourcesRank,
    `Faculty salaries` = facultySalaries,
    `Percent of faculty who are full-time` = facultyFT,
    `Classes with fewer than 20 students` = smallClasses,
    `Student-Faculty Ratio` = studentFacultyRatio,
    `Financial resources rank` = resourcesRank,
    `SAT/ACT 25th-75th percentile` = standardizedTests,

    `Contribution from Outcomes` = contribute.outcomesRank, 
    `Contribution from Average 6-year graduation rate` = contribute.gradRate6yr, 
    `Contribution from Average first-year student retention rate` = contribute.retentionRateFY,   
    `Contribution from 6-year graduation rate of students who received a Pell Grant` = contribute.gradRate6yrPell,
    `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` = contribute.gradRate6yrNoPell,
    `Contribution from Overperformance/Underperformance` = contribute.overunderPerform,
    `Contribution from Median federal loan debt for borrowers` = contribute.medianDebt,
    `Contribution from College grads earning more than a HS grad` = contribute.thresholdHS,
    `Contribution from Expert Opinion` = contribute.peerScore,
    `Contribution from Faculty Resources` = contribute.facultyResourcesRank,
    `Contribution from Faculty salaries` = contribute.facultySalaries,
    `Contribution from Percent of faculty who are full-time` = contribute.facultyFT,
    `Contribution from Student-Faculty Ratio` = contribute.studentFacultyRatio,
    `Contribution from Financial Resources` = contribute.resourcesRank,
    `Contribution from Student Excellence` = contribute.standardizedTests
  )
  
  rm(
    universityName,
    overallRanking,
    overallScore,
    outcomesRank,
    gradRate6yr,
    retentionRateFY,
    mobilityRank,
    gradRate6yrPell,
    gradRate6yrNoPell,
    predictedGradRate,
    overunderPerform,
    medianDebt,
    thresholdHS,
    peerScore,
    facultyResourcesRank,
    facultySalaries,
    facultyFT,
    smallClasses,
    studentFacultyRatio,
    resourcesRank,
    standardizedTests,
    contribute.outcomesRank, 
    contribute.gradRate6yr, 
    contribute.retentionRateFY,   
    contribute.gradRate6yrPell,
    contribute.gradRate6yrNoPell,
    contribute.overunderPerform,
    contribute.medianDebt,
    contribute.thresholdHS,
    contribute.peerScore,
    contribute.facultyResourcesRank,
    contribute.facultySalaries,
    contribute.facultyFT,
    contribute.studentFacultyRatio,
    contribute.resourcesRank,
    contribute.standardizedTests,
    inputData, 
    sheetName
  )
  
}
rm(i)

#### End #### 

#### Format contributions measures ####

nationalUniversities$`Contribution from Outcomes`[is.na(nationalUniversities$`Contribution from Outcomes`)] <- 0
nationalUniversities$`Contribution from Average 6-year graduation rate`[is.na(nationalUniversities$`Contribution from Average 6-year graduation rate`)] <- 0
nationalUniversities$`Contribution from Average first-year student retention rate`[is.na(nationalUniversities$`Contribution from Average first-year student retention rate`)] <- 0
nationalUniversities$`Contribution from 6-year graduation rate of students who received a Pell Grant`[is.na(nationalUniversities$`Contribution from 6-year graduation rate of students who received a Pell Grant`)] <- 0
nationalUniversities$`Contribution from 6-year graduation rate of students who did not receive a Pell Grant`[is.na(nationalUniversities$`Contribution from 6-year graduation rate of students who did not receive a Pell Grant`)] <- 0
nationalUniversities$`Contribution from 6-year graduation rate of first generation students`[is.na(nationalUniversities$`Contribution from 6-year graduation rate of first generation students`)] <- 0
nationalUniversities$`Contribution from 6-year graduation rate of non-first generation students`[is.na(nationalUniversities$`Contribution from 6-year graduation rate of non-first generation students`)] <- 0
nationalUniversities$`Contribution from Overperformance/Underperformance`[is.na(nationalUniversities$`Contribution from Overperformance/Underperformance`)] <- 0
nationalUniversities$`Contribution from Median federal loan debt for borrowers`[is.na(nationalUniversities$`Contribution from Median federal loan debt for borrowers`)] <- 0
nationalUniversities$`Contribution from College grads earning more than a HS grad`[is.na(nationalUniversities$`Contribution from College grads earning more than a HS grad`)] <- 0
nationalUniversities$`Contribution from Expert Opinion`[is.na(nationalUniversities$`Contribution from Expert Opinion`)] <- 0
nationalUniversities$`Contribution from Faculty Resources`[is.na(nationalUniversities$`Contribution from Faculty Resources`)] <- 0
nationalUniversities$`Contribution from Faculty salaries`[is.na(nationalUniversities$`Contribution from Faculty salaries`)] <- 0
nationalUniversities$`Contribution from Percent of faculty who are full-time`[is.na(nationalUniversities$`Contribution from Percent of faculty who are full-time`)] <- 0
nationalUniversities$`Contribution from Student-Faculty Ratio`[is.na(nationalUniversities$`Contribution from Student-Faculty Ratio`)] <- 0
nationalUniversities$`Contribution from Financial Resources`[is.na(nationalUniversities$`Contribution from Financial Resources`)] <- 0
nationalUniversities$`Contribution from Student Excellence`[is.na(nationalUniversities$`Contribution from Student Excellence`)] <- 0
nationalUniversities$`Contribution from Faculty Research`[is.na(nationalUniversities$`Contribution from Faculty Research`)] <- 0

nationalLAs$`Contribution from Outcomes`[is.na(nationalLAs$`Contribution from Outcomes`)] <- 0
nationalLAs$`Contribution from Average 6-year graduation rate`[is.na(nationalLAs$`Contribution from Average 6-year graduation rate`)] <- 0
nationalLAs$`Contribution from Average first-year student retention rate`[is.na(nationalLAs$`Contribution from Average first-year student retention rate`)] <- 0
nationalLAs$`Contribution from 6-year graduation rate of students who received a Pell Grant`[is.na(nationalLAs$`Contribution from 6-year graduation rate of students who received a Pell Grant`)] <- 0
nationalLAs$`Contribution from 6-year graduation rate of students who did not receive a Pell Grant`[is.na(nationalLAs$`Contribution from 6-year graduation rate of students who did not receive a Pell Grant`)] <- 0
nationalLAs$`Contribution from Overperformance/Underperformance`[is.na(nationalLAs$`Contribution from Overperformance/Underperformance` )] <- 0
nationalLAs$`Contribution from Median federal loan debt for borrowers`[is.na(nationalLAs$`Contribution from Median federal loan debt for borrowers`)] <- 0
nationalLAs$`Contribution from College grads earning more than a HS grad`[is.na(nationalLAs$`Contribution from College grads earning more than a HS grad`)] <- 0
nationalLAs$`Contribution from Expert Opinion` [is.na(nationalLAs$`Contribution from Expert Opinion`)] <- 0
nationalLAs$`Contribution from Faculty Resources`[is.na(nationalLAs$`Contribution from Faculty Resources`)] <- 0
nationalLAs$`Contribution from Faculty salaries` [is.na(nationalLAs$`Contribution from Faculty salaries`)] <- 0
nationalLAs$`Contribution from Percent of faculty who are full-time`[is.na(nationalLAs$`Contribution from Percent of faculty who are full-time`)] <- 0
nationalLAs$`Contribution from Student-Faculty Ratio`[is.na(nationalLAs$`Contribution from Student-Faculty Ratio`)] <- 0
nationalLAs$`Contribution from Financial Resources`[is.na(nationalLAs$`Contribution from Financial Resources`)] <- 0
nationalLAs$`Contribution from Student Excellence`[is.na(nationalLAs$`Contribution from Student Excellence`)] <- 0

nationalUniversities <- nationalUniversities %>% mutate(
  `Section-level contributions` = `Contribution from Outcomes` + `Contribution from Expert Opinion` + `Contribution from Faculty Resources` + `Contribution from Financial Resources` + `Contribution from Student Excellence` + `Contribution from Faculty Research`,
  `Composite contributions` = `Contribution from Average 6-year graduation rate` + `Contribution from Average first-year student retention rate` + `Contribution from 6-year graduation rate of students who received a Pell Grant` + `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` + `Contribution from 6-year graduation rate of first generation students` + `Contribution from 6-year graduation rate of non-first generation students` + `Contribution from Overperformance/Underperformance` + `Contribution from Median federal loan debt for borrowers` + `Contribution from College grads earning more than a HS grad` + `Contribution from Expert Opinion` + `Contribution from Faculty salaries` + `Contribution from Percent of faculty who are full-time` + `Contribution from Student-Faculty Ratio` + `Contribution from Financial Resources` + `Contribution from Student Excellence` + `Contribution from Faculty Research`
)

nationalLAs <- nationalLAs %>% mutate(
  `Section-level contributions` = `Contribution from Outcomes` + `Contribution from Expert Opinion` + `Contribution from Faculty Resources` + `Contribution from Financial Resources` + `Contribution from Student Excellence`, 
  `Composite contributions` = `Contribution from Average 6-year graduation rate` + `Contribution from Average first-year student retention rate` + `Contribution from 6-year graduation rate of students who received a Pell Grant` + `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` + `Contribution from Overperformance/Underperformance` + `Contribution from Median federal loan debt for borrowers` + `Contribution from College grads earning more than a HS grad` + `Contribution from Expert Opinion` + `Contribution from Faculty salaries` + `Contribution from Percent of faculty who are full-time` + `Contribution from Student-Faculty Ratio` + `Contribution from Financial Resources` + `Contribution from Student Excellence`
)

#### End #### 

#### Remove unranked colleges ####

nationalUniversities <- nationalUniversities %>% filter(is.na(`Overall ranking`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Overall ranking`)==FALSE)

nationalUniversities <- nationalUniversities %>% mutate(`Overall ranking` = rank(-`Overall score`, ties.method = "min"))
nationalLAs <- nationalLAs %>% mutate(`Overall ranking` = rank(-`Overall score`, ties.method = "min"))

test1 <- nationalUniversities %>% select(
  `Name`, `Overall ranking`, `Overall score`
) %>% mutate(
  `Rank of overall score` = rank(-`Overall score`, ties.method="min"), 
  `Rank of ranking` = rank(-`Overall ranking`, ties.method="min")
)
test2 <- nationalLAs %>% select(
  `Name`, `Overall ranking`, `Overall score`
) %>% mutate(
  `Rank of overall score` = rank(-`Overall score`, ties.method="min"), 
  `Rank of ranking` = rank(-`Overall ranking`, ties.method="min")
)

cor(test1$`Rank of overall score`, test1$`Rank of ranking`)
cor(test2$`Rank of overall score`, test2$`Rank of ranking`)

rm(test1, test2)

#### End #### 

#### Remove colleges with missing data ####

nationalUniversities <- nationalUniversities %>% filter(is.na(`Average 6-year graduation rate`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Average first-year student retention rate`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Social Mobility Rank`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`6-year graduation rate of students who received a Pell Grant`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`6-year graduation rate of students who did not receive a Pell Grant`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`6-year graduation rate of first generation students`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`6-year graduation rate of non-first generation students`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Predicted graduation rate`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Overperformance/Underperformance`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Median federal loan debt for borrowers`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`College grads earning more than a HS grad`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Peer assessment score`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Faculty resources rank`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Faculty salaries`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Percent of faculty who are full-time`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Classes with fewer than 20 students`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Student-Faculty Ratio`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Financial resources rank`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`SAT/ACT 25th-75th percentile`)==FALSE)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Bibliometric Rank`)==FALSE)

nationalLAs <- nationalLAs %>% filter(is.na(`Average 6-year graduation rate`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Average first-year student retention rate`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Social Mobility Rank`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`6-year graduation rate of students who received a Pell Grant`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`6-year graduation rate of students who did not receive a Pell Grant`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Predicted graduation rate`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Overperformance/Underperformance`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Median federal loan debt for borrowers`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`College grads earning more than a HS grad`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Peer assessment score`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Faculty resources rank`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Faculty salaries`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Percent of faculty who are full-time`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Classes with fewer than 20 students`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Student-Faculty Ratio`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`Financial resources rank`)==FALSE)
nationalLAs <- nationalLAs %>% filter(is.na(`SAT/ACT 25th-75th percentile`)==FALSE)

nationalUniversities <- nationalUniversities %>% mutate(`Overall ranking` = rank(-`Overall score`, ties.method = "min"))
nationalLAs <- nationalLAs %>% mutate(`Overall ranking` = rank(-`Overall score`, ties.method = "min"))

#### End #### 

################################################
#### Recreate rankings to confirm validity  ####
################################################

#### Establish variable lists for both ####

varListUniversities <- c(
  "Outcomes rank",
  "Average 6-year graduation rate",
  "Average first-year student retention rate",
  "Social Mobility Rank",
  "6-year graduation rate of students who received a Pell Grant",
  "6-year graduation rate of students who did not receive a Pell Grant",
  "6-year graduation rate of first generation students",
  "6-year graduation rate of non-first generation students",
  "Predicted graduation rate",
  "Overperformance/Underperformance",
  "Median federal loan debt for borrowers",
  "College grads earning more than a HS grad",
  "Peer assessment score",
  "Faculty resources rank",
  "Faculty salaries",
  "Percent of faculty who are full-time",
  "Classes with fewer than 20 students",
  "Student-Faculty Ratio",
  "Financial resources rank",
  "SAT/ACT 25th-75th percentile",
  "Bibliometric Rank"
)

varListLAs <- c(
  "Outcomes rank",
  "Average 6-year graduation rate",
  "Average first-year student retention rate",
  "Social Mobility Rank",
  "6-year graduation rate of students who received a Pell Grant",
  "6-year graduation rate of students who did not receive a Pell Grant",
  "Predicted graduation rate",
  "Overperformance/Underperformance",
  "Median federal loan debt for borrowers",
  "College grads earning more than a HS grad",
  "Peer assessment score",
  "Faculty resources rank",
  "Faculty salaries",
  "Percent of faculty who are full-time",
  "Classes with fewer than 20 students",
  "Student-Faculty Ratio",
  "Financial resources rank",
  "SAT/ACT 25th-75th percentile"
)

#### End #### 

#### Create Z-score variables ####

for(i in (1:length(varListUniversities))){
  
  varName <- varListUniversities[i]
  
  tempData <- nationalUniversities %>% select(
    `Name`,
    all_of(varName)
  )
  names(tempData) <- c("Name", "Selected Variable")
  tempData <- tempData %>% filter(is.na(`Selected Variable`)==FALSE)
  
  varMean <- mean(tempData$`Selected Variable`)
  varSD <- sd(tempData$`Selected Variable`)
  
  print(paste(varName, ": ", round(varMean, digits=2), round(varSD, digits=2)))
  
  tempData <- tempData %>% mutate(`Z-score` = (`Selected Variable` - varMean) / varSD)
  tempData <- tempData %>% select(-(`Selected Variable`))
  
  if((grepl("graduation rate", varName)==FALSE) & 
     (grepl("retention rate", varName)==FALSE) & 
     (grepl("performance", varName)==FALSE) & 
     (grepl("HS grad", varName)==FALSE) & 
     (grepl("Peer assessment", varName)==FALSE) & 
     (grepl("full-time", varName)==FALSE) & 
     (grepl("fewer than", varName)==FALSE) & 
     (grepl("percentile", varName)==FALSE)
  ){tempData <- tempData %>% mutate(`Z-score` = -(`Z-score`))}
  
  names(tempData)[2] <- paste("Z-Score: ", varName, sep="")
  
  nationalUniversities <- left_join(x=nationalUniversities, y=tempData, by="Name")
  
  rm(varName, tempData, varMean, varSD)
  
}
rm(i)

for(i in (1:length(varListLAs))){
  
  varName <- varListLAs[i]
  
  tempData <- nationalLAs %>% select(
    `Name`,
    all_of(varName)
  )
  names(tempData) <- c("Name", "Selected Variable")
  tempData <- tempData %>% filter(is.na(`Selected Variable`)==FALSE)
  
  varMean <- mean(tempData$`Selected Variable`)
  varSD <- sd(tempData$`Selected Variable`)
  
  print(paste(varName, ": ", round(varMean, digits=2), round(varSD, digits=2)))
  
  tempData <- tempData %>% mutate(`Z-score` = (`Selected Variable` - varMean) / varSD)
  tempData <- tempData %>% select(-(`Selected Variable`))
  
  if((grepl("graduation rate", varName)==FALSE) & 
     (grepl("retention rate", varName)==FALSE) & 
     (grepl("performance", varName)==FALSE) & 
     (grepl("HS grad", varName)==FALSE) & 
     (grepl("Peer assessment", varName)==FALSE) & 
     (grepl("full-time", varName)==FALSE) & 
     (grepl("fewer than", varName)==FALSE) & 
     (grepl("percentile", varName)==FALSE)
  ){tempData <- tempData %>% mutate(`Z-score` = -(`Z-score`))}
  
  names(tempData)[2] <- paste("Z-Score: ", varName, sep="")
  
  nationalLAs <- left_join(x=nationalLAs, y=tempData, by="Name")
  
  rm(varName, tempData, varMean, varSD)
  
}
rm(i)

#### End #### 

#### Create Y-score variables ####

for(i in (1:length(varListUniversities))){
  
  varName <- paste("Z-Score: ", varListUniversities[i], sep="")
  
  tempData <- nationalUniversities %>% select(
    `Name`,
    all_of(varName)
  )
  names(tempData) <- c("Name", "Selected Z-Score")
  tempData <- tempData %>% filter(is.na(`Selected Z-Score`)==FALSE)
  
  worstVal <- min(tempData$`Selected Z-Score`, na.rm=TRUE)
  bestVal <- max(tempData$`Selected Z-Score`, na.rm=TRUE)
  sepVal <- abs(bestVal - worstVal)
  
  tempData <- tempData %>% mutate(
    `Y-Score` = (`Selected Z-Score` - worstVal) * (100 / sepVal)
  )
  
  tempData <- tempData %>% select(-(`Selected Z-Score`))
  
  names(tempData)[2] <- paste("Y-Score: ", varListUniversities[i], sep="")
  
  nationalUniversities <- left_join(x=nationalUniversities, y=tempData, by="Name")
  
  rm(varName, tempData, worstVal, bestVal, sepVal)
}  
rm(i)

for(i in (1:length(varListLAs))){
  
  varName <- paste("Z-Score: ", varListLAs[i], sep="")
  
  tempData <- nationalLAs %>% select(
    `Name`,
    all_of(varName)
  )
  names(tempData) <- c("Name", "Selected Z-Score")
  tempData <- tempData %>% filter(is.na(`Selected Z-Score`)==FALSE)
  
  worstVal <- min(tempData$`Selected Z-Score`, na.rm=TRUE)
  bestVal <- max(tempData$`Selected Z-Score`, na.rm=TRUE)
  sepVal <- abs(bestVal - worstVal)
  
  tempData <- tempData %>% mutate(
    `Y-Score` = (`Selected Z-Score` - worstVal) * (100 / sepVal)
  )
  
  tempData <- tempData %>% select(-(`Selected Z-Score`))
  
  names(tempData)[2] <- paste("Y-Score: ", varListLAs[i], sep="")
  
  nationalLAs <- left_join(x=nationalLAs, y=tempData, by="Name")
  
  rm(varName, tempData, worstVal, bestVal, sepVal)
}  
rm(i)

#### End #### 

#### Confirm "best" universities are the same in each variable ####

for(i in (1:length(varListUniversities))){
  
  varName <- varListUniversities[i]
  
  print(varName)
  
  tempData <- nationalUniversities %>% select(
    `Name`, 
    all_of(varName), 
    all_of(paste("Y-Score: ", varName, sep=""))
  )
  names(tempData)[2] <- "Selected Variable"
  names(tempData)[3] <- "Selected Y-Score"
  
  tempData <- tempData %>% filter(is.na(`Selected Variable`)==FALSE)
  
  if((grepl("graduation rate", varName)) | 
     (grepl("retention rate", varName)) | 
     (grepl("performance", varName)) | 
     (grepl("HS grad", varName)) | 
     (grepl("Peer assessment", varName)) | 
     (grepl("full-time", varName)) | 
     (grepl("fewer than", varName)) | 
     (grepl("percentile", varName))
  ){
    tempData1 <- tempData %>% arrange(desc(`Selected Variable`))
  }else{
    tempData1 <- tempData %>% arrange(`Selected Variable`)
  }
  tempData2 <- tempData %>% arrange(desc(`Selected Y-Score`))
  
  print(tempData1$`Name`[1:4])
  print(tempData2$`Name`[1:4])
  
  rm(tempData, tempData1, tempData2, varName)

}
rm(i)

for(i in (1:length(varListLAs))){
  
  varName <- varListLAs[i]
  
  print(varName)
  
  tempData <- nationalLAs %>% select(
    `Name`, 
    all_of(varName), 
    all_of(paste("Y-Score: ", varName, sep=""))
  )
  names(tempData)[2] <- "Selected Variable"
  names(tempData)[3] <- "Selected Y-Score"
  
  tempData <- tempData %>% filter(is.na(`Selected Variable`)==FALSE)
  
  if((grepl("graduation rate", varName)) | 
     (grepl("retention rate", varName)) | 
     (grepl("performance", varName)) | 
     (grepl("HS grad", varName)) | 
     (grepl("Peer assessment", varName)) | 
     (grepl("full-time", varName)) | 
     (grepl("fewer than", varName)) | 
     (grepl("percentile", varName))
  ){
    tempData1 <- tempData %>% arrange(desc(`Selected Variable`))
  }else{
    tempData1 <- tempData %>% arrange(`Selected Variable`)
  }
  tempData2 <- tempData %>% arrange(desc(`Selected Y-Score`))
  
  print(tempData1$`Name`[1:4])
  print(tempData2$`Name`[1:4])
  
  rm(tempData, tempData1, tempData2, varName)
  
}
rm(i)


#### End #### 

#### Check correlations by variable ####

cor(nationalUniversities$`Outcomes rank`, nationalUniversities$`Y-Score: Outcomes rank`, use = "complete.obs")
cor(nationalUniversities$`Average 6-year graduation rate`, nationalUniversities$`Y-Score: Average 6-year graduation rate`, use = "complete.obs")
cor(nationalUniversities$`Average first-year student retention rate`, nationalUniversities$`Y-Score: Average first-year student retention rate`, use = "complete.obs")
cor(nationalUniversities$`Social Mobility Rank`, nationalUniversities$`Y-Score: Social Mobility Rank`, use = "complete.obs")
cor(nationalUniversities$`6-year graduation rate of students who received a Pell Grant`, nationalUniversities$`Y-Score: 6-year graduation rate of students who received a Pell Grant`, use = "complete.obs")
cor(nationalUniversities$`6-year graduation rate of students who did not receive a Pell Grant`, nationalUniversities$`Y-Score: 6-year graduation rate of students who did not receive a Pell Grant`, use = "complete.obs")
cor(nationalUniversities$`6-year graduation rate of first generation students`, nationalUniversities$`Y-Score: 6-year graduation rate of first generation students`, use = "complete.obs")
cor(nationalUniversities$`6-year graduation rate of non-first generation students`, nationalUniversities$`Y-Score: 6-year graduation rate of non-first generation students`, use = "complete.obs")
cor(nationalUniversities$`Predicted graduation rate`, nationalUniversities$`Y-Score: Predicted graduation rate`, use = "complete.obs")
cor(nationalUniversities$`Overperformance/Underperformance`, nationalUniversities$`Y-Score: Overperformance/Underperformance`, use = "complete.obs")
cor(nationalUniversities$`Median federal loan debt for borrowers`, nationalUniversities$`Y-Score: Median federal loan debt for borrowers`, use = "complete.obs")
cor(nationalUniversities$`College grads earning more than a HS grad`, nationalUniversities$`Y-Score: College grads earning more than a HS grad`, use = "complete.obs")
cor(nationalUniversities$`Peer assessment score`, nationalUniversities$`Y-Score: Peer assessment score`, use = "complete.obs")
cor(nationalUniversities$`Faculty resources rank`, nationalUniversities$`Y-Score: Faculty resources rank`, use = "complete.obs")
cor(nationalUniversities$`Faculty salaries`, nationalUniversities$`Y-Score: Faculty salaries`, use = "complete.obs")
cor(nationalUniversities$`Percent of faculty who are full-time`, nationalUniversities$`Y-Score: Percent of faculty who are full-time`, use = "complete.obs")
cor(nationalUniversities$`Classes with fewer than 20 students`, nationalUniversities$`Y-Score: Classes with fewer than 20 students`, use = "complete.obs")
cor(nationalUniversities$`Student-Faculty Ratio`, nationalUniversities$`Y-Score: Student-Faculty Ratio`, use = "complete.obs")
cor(nationalUniversities$`Financial resources rank`, nationalUniversities$`Y-Score: Financial resources rank`, use = "complete.obs")
cor(nationalUniversities$`SAT/ACT 25th-75th percentile`, nationalUniversities$`Y-Score: SAT/ACT 25th-75th percentile`, use = "complete.obs")
cor(nationalUniversities$`Bibliometric Rank`, nationalUniversities$`Y-Score: Bibliometric Rank`, use = "complete.obs")

cor(nationalLAs$`Outcomes rank`, nationalLAs$`Y-Score: Outcomes rank`, use = "complete.obs")
cor(nationalLAs$`Average 6-year graduation rate`, nationalLAs$`Y-Score: Average 6-year graduation rate`, use = "complete.obs")
cor(nationalLAs$`Average first-year student retention rate`, nationalLAs$`Y-Score: Average first-year student retention rate`, use = "complete.obs")
cor(nationalLAs$`Social Mobility Rank`, nationalLAs$`Y-Score: Social Mobility Rank`, use = "complete.obs")
cor(nationalLAs$`6-year graduation rate of students who received a Pell Grant`, nationalLAs$`Y-Score: 6-year graduation rate of students who received a Pell Grant`, use = "complete.obs")
cor(nationalLAs$`6-year graduation rate of students who did not receive a Pell Grant`, nationalLAs$`Y-Score: 6-year graduation rate of students who did not receive a Pell Grant`, use = "complete.obs")
cor(nationalLAs$`Predicted graduation rate`, nationalLAs$`Y-Score: Predicted graduation rate`, use = "complete.obs")
cor(nationalLAs$`Overperformance/Underperformance`, nationalLAs$`Y-Score: Overperformance/Underperformance`, use = "complete.obs")
cor(nationalLAs$`Median federal loan debt for borrowers`, nationalLAs$`Y-Score: Median federal loan debt for borrowers`, use = "complete.obs")
cor(nationalLAs$`College grads earning more than a HS grad`, nationalLAs$`Y-Score: College grads earning more than a HS grad`, use = "complete.obs")
cor(nationalLAs$`Peer assessment score`, nationalLAs$`Y-Score: Peer assessment score`, use = "complete.obs")
cor(nationalLAs$`Faculty resources rank`, nationalLAs$`Y-Score: Faculty resources rank`, use = "complete.obs")
cor(nationalLAs$`Faculty salaries`, nationalLAs$`Y-Score: Faculty salaries`, use = "complete.obs")
cor(nationalLAs$`Percent of faculty who are full-time`, nationalLAs$`Y-Score: Percent of faculty who are full-time`, use = "complete.obs")
cor(nationalLAs$`Classes with fewer than 20 students`, nationalLAs$`Y-Score: Classes with fewer than 20 students`, use = "complete.obs")
cor(nationalLAs$`Student-Faculty Ratio`, nationalLAs$`Y-Score: Student-Faculty Ratio`, use = "complete.obs")
cor(nationalLAs$`Financial resources rank`, nationalLAs$`Y-Score: Financial resources rank`, use = "complete.obs")
cor(nationalLAs$`SAT/ACT 25th-75th percentile`, nationalLAs$`Y-Score: SAT/ACT 25th-75th percentile`, use = "complete.obs")

#### End #### 

#### Make sure NA-inputs have 0-contributions ####

nationalUniversities$`Contribution from Student Excellence`[is.na(nationalUniversities$`Y-Score: SAT/ACT 25th-75th percentile`)] <- 0
nationalLAs$`Contribution from Student Excellence`[is.na(nationalLAs$`Y-Score: SAT/ACT 25th-75th percentile`)] <- 0

#### End #### 

#### Recreate overall scores ####

nationalUniversities$`Y-Score: SAT/ACT 25th-75th percentile`[is.na(nationalUniversities$`Y-Score: SAT/ACT 25th-75th percentile`)] <- 0
nationalUniversities <- nationalUniversities %>% mutate(
  `Recreating Overall Score` = (
    `Y-Score: Average 6-year graduation rate` * `Contribution from Average 6-year graduation rate`
  ) + (
    `Y-Score: Average first-year student retention rate` * `Contribution from Average first-year student retention rate`
  ) + (
    `Y-Score: 6-year graduation rate of students who received a Pell Grant` * `Contribution from 6-year graduation rate of students who received a Pell Grant`
  ) + (
    `Y-Score: 6-year graduation rate of students who did not receive a Pell Grant` * `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` 
  ) + (
    `Y-Score: 6-year graduation rate of first generation students` * `Contribution from 6-year graduation rate of first generation students`
  ) + (
    `Y-Score: 6-year graduation rate of non-first generation students` * `Contribution from 6-year graduation rate of non-first generation students`
  ) + (
    `Y-Score: Overperformance/Underperformance` * `Contribution from Overperformance/Underperformance`
  ) + (
    `Y-Score: Median federal loan debt for borrowers` * `Contribution from Median federal loan debt for borrowers`
  ) + (
    `Y-Score: College grads earning more than a HS grad` * `Contribution from College grads earning more than a HS grad`
  ) + (
    `Y-Score: Peer assessment score` * `Contribution from Expert Opinion`
  ) + (
    `Y-Score: Faculty salaries` * `Contribution from Faculty salaries`
  ) + (
    `Y-Score: Percent of faculty who are full-time` * `Contribution from Percent of faculty who are full-time`
  ) + (
    `Y-Score: Student-Faculty Ratio` * `Contribution from Student-Faculty Ratio` 
  ) + (
    `Y-Score: Financial resources rank` * `Contribution from Financial Resources`
  ) + (
    `Y-Score: SAT/ACT 25th-75th percentile` * `Contribution from Student Excellence`
  ) + (
    `Y-Score: Bibliometric Rank` * `Contribution from Faculty Research`
  ) 
)
nationalUniversities <- nationalUniversities %>% filter(is.na(`Recreating Overall Score`)==FALSE)

nationalLAs$`Y-Score: SAT/ACT 25th-75th percentile`[is.na(nationalLAs$`Y-Score: SAT/ACT 25th-75th percentile`)] <- 0
nationalLAs <- nationalLAs %>% mutate(
  `Recreating Overall Score` = (
    `Y-Score: Average 6-year graduation rate` * `Contribution from Average 6-year graduation rate`
  ) + (
    `Y-Score: Average first-year student retention rate` * `Contribution from Average first-year student retention rate`
  ) + (
    `Y-Score: 6-year graduation rate of students who received a Pell Grant` * `Contribution from 6-year graduation rate of students who received a Pell Grant`
  ) + (
    `Y-Score: 6-year graduation rate of students who did not receive a Pell Grant` * `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` 
  ) + (
    `Y-Score: Overperformance/Underperformance` * `Contribution from Overperformance/Underperformance`
  ) + (
    `Y-Score: Median federal loan debt for borrowers` * `Contribution from Median federal loan debt for borrowers`
  ) + (
    `Y-Score: College grads earning more than a HS grad` * `Contribution from College grads earning more than a HS grad`
  ) + (
    `Y-Score: Peer assessment score` * `Contribution from Expert Opinion`
  ) + (
    `Y-Score: Faculty salaries` * `Contribution from Faculty salaries`
  ) + (
    `Y-Score: Percent of faculty who are full-time` * `Contribution from Percent of faculty who are full-time`
  ) + (
    `Y-Score: Student-Faculty Ratio` * `Contribution from Student-Faculty Ratio` 
  ) + (
    `Y-Score: Financial resources rank` * `Contribution from Financial Resources`
  ) + (
    `Y-Score: SAT/ACT 25th-75th percentile` * `Contribution from Student Excellence`
  ) 
)
nationalLAs <- nationalLAs %>% filter(is.na(`Recreating Overall Score`)==FALSE)

#### End #### 

#### Recreate overall rankings ####

nationalUniversities <- nationalUniversities %>% mutate(
  `Recreating Overall Ranking` = rank(-`Recreating Overall Score`, ties.method="min")
)
nationalLAs <- nationalLAs %>% mutate(
  `Recreating Overall Ranking` = rank(-`Recreating Overall Score`, ties.method="min")
)

#### End #### 

#### Check correlations ####

cor(x=nationalUniversities$`Overall score`, y=nationalUniversities$`Recreating Overall Score`, use = "complete.obs")
cor(x=nationalLAs$`Overall score`, y=nationalLAs$`Recreating Overall Score`, use = "complete.obs")
cor(x=nationalUniversities$`Overall ranking`, y=nationalUniversities$`Recreating Overall Ranking`, use = "complete.obs")
cor(x=nationalLAs$`Overall ranking`, y=nationalLAs$`Recreating Overall Ranking`, use = "complete.obs")

rankCheck1 <- nationalUniversities %>% select(
  `Name`, `Overall score`, `Overall ranking`, `Recreating Overall Score`, `Recreating Overall Ranking`
) %>% mutate(
  `Ranking difference` = abs(`Overall ranking` - `Recreating Overall Ranking`)
)
rankCheck2 <- nationalLAs %>% select(
  `Name`, `Overall score`, `Overall ranking`, `Recreating Overall Score`, `Recreating Overall Ranking`
) %>% mutate(
  `Ranking difference` = abs(`Overall ranking` - `Recreating Overall Ranking`)
)

summary(rankCheck1$`Ranking difference`)
summary(rankCheck2$`Ranking difference`)

summary(rankCheck1$`Overall ranking`)
summary(rankCheck1$`Recreating Overall Ranking`)
summary(rankCheck2$`Overall ranking`)
summary(rankCheck2$`Recreating Overall Ranking`)

rm(rankCheck1,  rankCheck2)

#### End #### 

#### Replace published rankings with recreated rankings ####

nationalUniversities <- nationalUniversities %>% mutate(
  `Overall score` = `Recreating Overall Score`, 
  `Overall ranking` = `Recreating Overall Ranking`
) %>% select(
  -(`Recreating Overall Score`), 
  -(`Recreating Overall Ranking`)
)

nationalLAs <- nationalLAs %>% mutate(
  `Overall score` = `Recreating Overall Score`, 
  `Overall ranking` = `Recreating Overall Ranking`
) %>% select(
  -(`Recreating Overall Score`), 
  -(`Recreating Overall Ranking`)
)

#### End #### 

################################################
#### Simulate changes to key measures       ####
################################################

#### Snapshot data ####

nationalUniversities.snapshot <- nationalUniversities %>% select(
  `Name`,
  `Overall ranking`,
  `Overall score`,
  `Outcomes rank`,
  `Average 6-year graduation rate`, 
  `Average first-year student retention rate`, 
  `Social Mobility Rank`,
  `6-year graduation rate of students who received a Pell Grant`, 
  `6-year graduation rate of students who did not receive a Pell Grant`, 
  `6-year graduation rate of first generation students`, 
  `6-year graduation rate of non-first generation students`, 
  `Predicted graduation rate`,
  `Overperformance/Underperformance`, 
  `Median federal loan debt for borrowers`, 
  `College grads earning more than a HS grad`, 
  `Peer assessment score`, 
  `Faculty resources rank`,
  `Faculty salaries`, 
  `Percent of faculty who are full-time`, 
  `Classes with fewer than 20 students`,
  `Student-Faculty Ratio`, 
  `Financial resources rank`, 
  `SAT/ACT 25th-75th percentile`, 
  `Bibliometric Rank`, 
  `Contribution from Average 6-year graduation rate`, 
  `Contribution from Average first-year student retention rate`, 
  `Contribution from 6-year graduation rate of students who received a Pell Grant`,
  `Contribution from 6-year graduation rate of students who did not receive a Pell Grant`,
  `Contribution from 6-year graduation rate of first generation students`,
  `Contribution from 6-year graduation rate of non-first generation students`,
  `Contribution from Overperformance/Underperformance`,
  `Contribution from Median federal loan debt for borrowers`,
  `Contribution from College grads earning more than a HS grad`,
  `Contribution from Expert Opinion`,
  `Contribution from Faculty salaries`,
  `Contribution from Percent of faculty who are full-time`,
  `Contribution from Student-Faculty Ratio`,
  `Contribution from Financial Resources`,
  `Contribution from Student Excellence`,
  `Contribution from Faculty Research`
)
nationalLAs.snapshot <- nationalLAs %>% select(
  `Name`,
  `Overall ranking`,
  `Overall score`,
  `Outcomes rank`,
  `Average 6-year graduation rate`, 
  `Average first-year student retention rate`, 
  `Social Mobility Rank`,
  `6-year graduation rate of students who received a Pell Grant`, 
  `6-year graduation rate of students who did not receive a Pell Grant`, 
  `Predicted graduation rate`,
  `Overperformance/Underperformance`, 
  `Median federal loan debt for borrowers`, 
  `College grads earning more than a HS grad`, 
  `Peer assessment score`, 
  `Faculty resources rank`,
  `Faculty salaries`, 
  `Percent of faculty who are full-time`, 
  `Classes with fewer than 20 students`,
  `Student-Faculty Ratio`, 
  `Financial resources rank`, 
  `SAT/ACT 25th-75th percentile`, 
  `Contribution from Average 6-year graduation rate`, 
  `Contribution from Average first-year student retention rate`, 
  `Contribution from 6-year graduation rate of students who received a Pell Grant`,
  `Contribution from 6-year graduation rate of students who did not receive a Pell Grant`,
  `Contribution from Overperformance/Underperformance`,
  `Contribution from Median federal loan debt for borrowers`,
  `Contribution from College grads earning more than a HS grad`,
  `Contribution from Expert Opinion`,
  `Contribution from Faculty salaries`,
  `Contribution from Percent of faculty who are full-time`,
  `Contribution from Student-Faculty Ratio`,
  `Contribution from Financial Resources`,
  `Contribution from Student Excellence`
)

#### End #### 

#### Establish variable lists for both ####

varListUniversities2 <- c(
  "Average 6-year graduation rate",
  "Average first-year student retention rate",
  "6-year graduation rate of students who received a Pell Grant",
  "6-year graduation rate of students who did not receive a Pell Grant",
  "6-year graduation rate of first generation students",
  "6-year graduation rate of non-first generation students",
  "Overperformance/Underperformance",
  "Median federal loan debt for borrowers",
  "College grads earning more than a HS grad",
  "Peer assessment score",
  "Faculty resources rank",
  "Faculty salaries",
  "Percent of faculty who are full-time",
  "Classes with fewer than 20 students",
  "Student-Faculty Ratio",
  "Financial resources rank",
  "SAT/ACT 25th-75th percentile",
  "Bibliometric Rank"
)

varListLAs2 <- c(
  "Outcomes rank",
  "Average 6-year graduation rate",
  "Average first-year student retention rate",
  "Social Mobility Rank",
  "6-year graduation rate of students who received a Pell Grant",
  "6-year graduation rate of students who did not receive a Pell Grant",
  "Predicted graduation rate",
  "Overperformance/Underperformance",
  "Median federal loan debt for borrowers",
  "College grads earning more than a HS grad",
  "Peer assessment score",
  "Faculty resources rank",
  "Faculty salaries",
  "Percent of faculty who are full-time",
  "Classes with fewer than 20 students",
  "Student-Faculty Ratio",
  "Financial resources rank",
  "SAT/ACT 25th-75th percentile"
)

#### End #### 

#### Write function to simulate single-college effect #### 

singleCollegeEffect <- function(selectedDF, collegeName, adjustingVar, adjustmentVal){
  
  if(selectedDF=="National universities"){
    tempDF <- nationalUniversities.snapshot
    oldRanking <- nationalUniversities
  }else{
    if(selectedDF=="National liberal arts colleges"){
      tempDF <- nationalLAs.snapshot %>% mutate(
        `6-year graduation rate of first generation students` = rep(0), 
        `6-year graduation rate of non-first generation students` = rep(0), 
        `Bibliometric Rank` = rep(0), 
        `Contribution from 6-year graduation rate of first generation students` = rep(0),
        `Contribution from 6-year graduation rate of non-first generation students` = rep(0),
        `Contribution from Faculty Research` = rep(0)
      )
      oldRanking <- nationalLAs
    }else{
      print("Error: Fix first argument")
      stop()
    }
  }
  
  tempDF1 <- tempDF %>% filter(`Name` == collegeName) 
  tempDF2 <- tempDF %>% filter(`Name` != collegeName)
  
  if(is.na(tempDF2$`Overall ranking`[1])){
    print("Error: Institution is not ranked")
    stop()
  }
  
  if(adjustingVar=="Average 6-year graduation rate"){ 
    tempDF1 <- tempDF1 %>% mutate(`Average 6-year graduation rate` = `Average 6-year graduation rate` + adjustmentVal)
  }
  if(adjustingVar=="Average first-year student retention rate"){ 
    tempDF1 <- tempDF1 %>% mutate(`Average first-year student retention rate` = `Average first-year student retention rate` + adjustmentVal)
  }
  if(adjustingVar=="6-year graduation rate of students who received a Pell Grant"){ 
    tempDF1 <- tempDF1 %>% mutate(`6-year graduation rate of students who received a Pell Grant` = `6-year graduation rate of students who received a Pell Grant` + adjustmentVal)
  }
  if(adjustingVar=="6-year graduation rate of students who did not receive a Pell Grant"){ 
    tempDF1 <- tempDF1 %>% mutate(`6-year graduation rate of students who did not receive a Pell Grant` = `6-year graduation rate of students who did not receive a Pell Grant` + adjustmentVal)
  }
  if(adjustingVar=="6-year graduation rate of first generation students"){ 
    tempDF1 <- tempDF1 %>% mutate(`6-year graduation rate of first generation students` = `6-year graduation rate of first generation students` + adjustmentVal)
  }
  if(adjustingVar=="6-year graduation rate of non-first generation students"){ 
    tempDF1 <- tempDF1 %>% mutate(`6-year graduation rate of non-first generation students` = `6-year graduation rate of non-first generation students` + adjustmentVal)
  }
  if(adjustingVar=="Overperformance/Underperformance"){ 
    tempDF1 <- tempDF1 %>% mutate(`Overperformance/Underperformance` = `Overperformance/Underperformance` + adjustmentVal)
  }
  if(adjustingVar=="Median federal loan debt for borrowers"){ 
    tempDF1 <- tempDF1 %>% mutate(`Median federal loan debt for borrowers` = `Median federal loan debt for borrowers` + adjustmentVal)
  }
  if(adjustingVar=="College grads earning more than a HS grad"){ 
    tempDF1 <- tempDF1 %>% mutate(`College grads earning more than a HS grad` = `College grads earning more than a HS grad` + adjustmentVal)
  }
  if(adjustingVar=="Peer assessment score"){ 
    tempDF1 <- tempDF1 %>% mutate(`Peer assessment score` = `Peer assessment score` + adjustmentVal)
  }
  if(adjustingVar=="Faculty resources rank"){ 
    tempDF1 <- tempDF1 %>% mutate(`Faculty resources rank` = `Faculty resources rank` + adjustmentVal)
  }
  if(adjustingVar=="Faculty salaries"){ 
    tempDF1 <- tempDF1 %>% mutate(`Faculty salaries` = `Faculty salaries` + adjustmentVal)
  }
  if(adjustingVar=="Percent of faculty who are full-time"){ 
    tempDF1 <- tempDF1 %>% mutate(`Percent of faculty who are full-time` = `Percent of faculty who are full-time` + adjustmentVal)
  }
  if(adjustingVar=="Student-Faculty Ratio"){ 
    tempDF1 <- tempDF1 %>% mutate(`Student-Faculty Ratio` = `Student-Faculty Ratio` + adjustmentVal)
  }
  if(adjustingVar=="Financial resources rank"){ 
    tempDF1 <- tempDF1 %>% mutate(`Financial resources rank` = `Financial resources rank` + adjustmentVal)
  }
  if(adjustingVar=="SAT/ACT 25th-75th percentile"){ 
    tempDF1 <- tempDF1 %>% mutate(`SAT/ACT 25th-75th percentile` = `SAT/ACT 25th-75th percentile` + adjustmentVal)
  }
  if(adjustingVar=="Bibliometric Rank"){ 
    tempDF1 <- tempDF1 %>% mutate(`Bibliometric Rank` = `Bibliometric Rank` + adjustmentVal)
  }
  
  tempDF <- rbind(tempDF1, tempDF2)
  rm(tempDF1, tempDF2)
  
  #### Create Z-score variables ####
  
  for(i in (1:length(varListUniversities2))){
    
    varName <- varListUniversities2[i]
    
    tempData <- tempDF %>% select(
      `Name`,
      all_of(varName)
    )
    names(tempData) <- c("Name", "Selected Variable")
    tempData <- tempData %>% filter(is.na(`Selected Variable`)==FALSE)
    
    varMean <- mean(tempData$`Selected Variable`)
    varSD <- sd(tempData$`Selected Variable`)
    
    if((selectedDF=="National liberal arts colleges") & (varName %in% c(
      "6-year graduation rate of first generation students", 
      "6-year graduation rate of non-first generation students", 
      "Bibliometric Rank"
    ))){
      varSD <- 1
    }
    
    tempData <- tempData %>% mutate(`Z-score` = (`Selected Variable` - varMean) / varSD)
    tempData <- tempData %>% select(-(`Selected Variable`))
    
    if((grepl("graduation rate", varName)==FALSE) & 
       (grepl("retention rate", varName)==FALSE) & 
       (grepl("performance", varName)==FALSE) & 
       (grepl("HS grad", varName)==FALSE) & 
       (grepl("Peer assessment", varName)==FALSE) & 
       (grepl("full-time", varName)==FALSE) & 
       (grepl("fewer than", varName)==FALSE) & 
       (grepl("percentile", varName)==FALSE)
    ){tempData <- tempData %>% mutate(`Z-score` = -(`Z-score`))}
    
    names(tempData)[2] <- paste("Z-Score: ", varName, sep="")
    
    tempDF <- left_join(x=tempDF, y=tempData, by="Name")
    
    rm(varName, tempData, varMean, varSD)
    
  }
  rm(i)
  
  #### Create Y-score variables ####
  
  for(i in (1:length(varListUniversities2))){
    
    varName <- paste("Z-Score: ", varListUniversities2[i], sep="")
    
    tempData <- tempDF %>% select(
      `Name`,
      all_of(varName)
    )
    names(tempData) <- c("Name", "Selected Z-Score")
    tempData <- tempData %>% filter(is.na(`Selected Z-Score`)==FALSE)
    
    worstVal <- min(tempData$`Selected Z-Score`, na.rm=TRUE)
    bestVal <- max(tempData$`Selected Z-Score`, na.rm=TRUE)
    sepVal <- abs(bestVal - worstVal)
    
    if((selectedDF=="National liberal arts colleges") & (varName %in% c(
      "Z-Score: 6-year graduation rate of first generation students", 
      "Z-Score: 6-year graduation rate of non-first generation students", 
      "Z-Score: Bibliometric Rank"
    ))){
      worstVal <- -3
      bestVal <- 3
      sepVal <- 6
    }
    
    tempData <- tempData %>% mutate(
      `Y-Score` = (`Selected Z-Score` - worstVal) * (100 / sepVal)
    )
    
    tempData <- tempData %>% select(-(`Selected Z-Score`))
    
    names(tempData)[2] <- paste("Y-Score: ", varListUniversities2[i], sep="")
    
    tempDF <- left_join(x=tempDF, y=tempData, by="Name")
    
    rm(varName, tempData, worstVal, bestVal, sepVal)
  }  
  rm(i)
  
  #### Recreate overall scores ####
  
  tempDF$`Y-Score: SAT/ACT 25th-75th percentile`[is.na(tempDF$`Y-Score: SAT/ACT 25th-75th percentile`)] <- 0
  tempDF <- tempDF %>% mutate(
    `Recreating Overall Score` = (
      `Y-Score: Average 6-year graduation rate` * `Contribution from Average 6-year graduation rate`
    ) + (
      `Y-Score: Average first-year student retention rate` * `Contribution from Average first-year student retention rate`
    ) + (
      `Y-Score: 6-year graduation rate of students who received a Pell Grant` * `Contribution from 6-year graduation rate of students who received a Pell Grant`
    ) + (
      `Y-Score: 6-year graduation rate of students who did not receive a Pell Grant` * `Contribution from 6-year graduation rate of students who did not receive a Pell Grant` 
    ) + (
      `Y-Score: 6-year graduation rate of first generation students` * `Contribution from 6-year graduation rate of first generation students`
    ) + (
      `Y-Score: 6-year graduation rate of non-first generation students` * `Contribution from 6-year graduation rate of non-first generation students`
    ) + (
      `Y-Score: Overperformance/Underperformance` * `Contribution from Overperformance/Underperformance`
    ) + (
      `Y-Score: Median federal loan debt for borrowers` * `Contribution from Median federal loan debt for borrowers`
    ) + (
      `Y-Score: College grads earning more than a HS grad` * `Contribution from College grads earning more than a HS grad`
    ) + (
      `Y-Score: Peer assessment score` * `Contribution from Expert Opinion`
    ) + (
      `Y-Score: Faculty salaries` * `Contribution from Faculty salaries`
    ) + (
      `Y-Score: Percent of faculty who are full-time` * `Contribution from Percent of faculty who are full-time`
    ) + (
      `Y-Score: Student-Faculty Ratio` * `Contribution from Student-Faculty Ratio` 
    ) + (
      `Y-Score: Financial resources rank` * `Contribution from Financial Resources`
    ) + (
      `Y-Score: SAT/ACT 25th-75th percentile` * `Contribution from Student Excellence`
    ) + (
      `Y-Score: Bibliometric Rank` * `Contribution from Faculty Research`
    ) 
  )
  tempDF <- tempDF %>% filter(is.na(`Recreating Overall Score`)==FALSE)
  
  #### Recreate overall rankings ####
  
  tempDF <- tempDF %>% mutate(
    `Recreating Overall Ranking` = rank(-`Recreating Overall Score`, ties.method="min")
  )
  
  # Return results
  oldRanking <- oldRanking %>% filter(`Name`==collegeName)
  oldRanking <- oldRanking$`Overall ranking`[1]
  newRanking <- tempDF %>% filter(`Name`==collegeName)
  newRanking <- newRanking$`Recreating Overall Ranking`[1]
  
  tempDF <- tempDF %>% filter(`Name`==collegeName)
  
  outputDF <- data.frame(
    `College name` = c(collegeName), 
    `Adjusted variable` = c(adjustingVar), 
    `Adjustment size` = c(adjustmentVal),
    `Pre-adjustment ranking` = c(oldRanking), 
    `Post-adjustment ranking` = c(newRanking),
    `Difference` = c((newRanking - oldRanking)),
    check.names=FALSE
  )
  
  return(outputDF)
  
  rm(oldRanking, newRanking, tempDF, outputDF)
}

#### End #### 

#### Write function to run prior function for all colleges ####

allCollegesOneEffect <- function(selectedDF0, adjustingVar0, adjustmentVal0){
  
  if(selectedDF0=="National universities"){
    for(i in (1:length(nationalUniversities$`Name`))){
      if(i==1){
        tempDF <- singleCollegeEffect(selectedDF="National universities", collegeName=nationalUniversities$`Name`[i], adjustingVar=adjustingVar0, adjustmentVal=adjustmentVal0)
      }else{
        tempDF2 <- singleCollegeEffect(selectedDF="National universities", collegeName=nationalUniversities$`Name`[i], adjustingVar=adjustingVar0, adjustmentVal=adjustmentVal0)
        tempDF <- rbind(tempDF, tempDF2)
        rm(tempDF2)
      }
    }
    rm(i)
    return(tempDF)
  }else{
    if(selectedDF0=="National liberal arts colleges"){
      for(i in (1:length(nationalLAs$`Name`))){
        if(i==1){
          tempDF <- singleCollegeEffect(selectedDF="National liberal arts colleges", collegeName=nationalLAs$`Name`[i], adjustingVar=adjustingVar0, adjustmentVal=adjustmentVal0)
        }else{
          tempDF2 <- singleCollegeEffect(selectedDF="National liberal arts colleges", collegeName=nationalLAs$`Name`[i], adjustingVar=adjustingVar0, adjustmentVal=adjustmentVal0)
          tempDF <- rbind(tempDF, tempDF2)
          rm(tempDF2)
        }
      }
      rm(i)
      return(tempDF)
    }else{
      print("Error: Fix first argument")
      stop()
    }
  }
}

#### End #### 

#### Run function #### 

testUniversities <- rbind(
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Average 6-year graduation rate", adjustmentVal0=-0.05), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Average 6-year graduation rate", adjustmentVal0=-0.1), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Average 6-year graduation rate", adjustmentVal0=-0.15), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Average first-year student retention rate", adjustmentVal0=-0.05), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Average first-year student retention rate", adjustmentVal0=-0.1), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Average first-year student retention rate", adjustmentVal0=-0.15), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=2500), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=5000), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=7500), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=10000),
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=15000), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-20), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-40), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-60), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-80), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-100), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-120), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-140), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-160),
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-180), 
  allCollegesOneEffect(selectedDF0="National universities", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-200)
)

testLAs <- rbind(
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Average 6-year graduation rate", adjustmentVal0=-0.05), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Average 6-year graduation rate", adjustmentVal0=-0.1), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Average 6-year graduation rate", adjustmentVal0=-0.15), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Average first-year student retention rate", adjustmentVal0=-0.05), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Average first-year student retention rate", adjustmentVal0=-0.1), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Average first-year student retention rate", adjustmentVal0=-0.15), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=2500), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=5000), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=7500), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=10000),
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="Median federal loan debt for borrowers", adjustmentVal0=15000), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-20), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-40), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-60), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-80), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-100), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-120), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-140), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-160),
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-180), 
  allCollegesOneEffect(selectedDF0="National liberal arts colleges", adjustingVar0="SAT/ACT 25th-75th percentile", adjustmentVal0=-200)
)

test1 <- aggregate(data=testUniversities, `Difference` ~ `Adjusted variable` + `Adjustment size`, FUN=mean)
test2 <- aggregate(data=testLAs, `Difference` ~ `Adjusted variable` + `Adjustment size`, FUN=mean)

write.csv(testUniversities, "output 08-28-2024-1.csv", row.names=FALSE)
write.csv(testLAs, "output 08-28-2024-2.csv", row.names=FALSE)

#### End #### 

################################################
#### Visualize results                      ####
################################################

#### Format results ####

testUniversities <- testUniversities %>% mutate(
  `Size of decline` = rep(NA)
)
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(0)] <- "No change"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(1)] <- "1 spot"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(2)] <- "2 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(3)] <- "3 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(4)] <- "4 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(5)] <- "5 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(6)] <- "6 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(7)] <- "7 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(8)] <- "8 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(9)] <- "9 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(10)] <- "10 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(11)] <- "11 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(12, 13)] <- "12-13 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(14, 15, 16)] <- "14-16 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` %in% c(17, 18, 19, 20)] <- "17-20 spots"
testUniversities$`Size of decline`[testUniversities$`Difference` > 20] <- "More than 20 spots"
testUniversities$`Size of decline` <- factor(testUniversities$`Size of decline`) 
testUniversities$`Size of decline` <- factor(testUniversities$`Size of decline`, levels=c(
  "No change",
  "1 spot",
  "2 spots",
  "3 spots",
  "4 spots",
  "5 spots",
  "6 spots",
  "7 spots",
  "8 spots",
  "9 spots",
  "10 spots",
  "11 spots",
  "12-13 spots",
  "14-16 spots",
  "17-20 spots",
  "More than 20 spots"
)) 
testUniversities <- testUniversities %>% mutate(`Count` = rep(1))
testUniversities <- testUniversities %>% mutate(`Adjustment size` = ifelse(
  `Adjusted variable` == "SAT/ACT 25th-75th percentile", round(`Adjustment size`, 0), ifelse(
    `Adjusted variable` == "Median federal loan debt for borrowers", paste("+", dollar(`Adjustment size`, 1), sep=""), ifelse(
      `Adjusted variable` %in% c("Average 6-year graduation rate", "Average first-year student retention rate"), percent(`Adjustment size`, accuracy=0.1), "")
    )
  )
)
testUniversities <- testUniversities %>% mutate(`Adjustment` = paste(`Adjusted variable`, ": ", `Adjustment size`, sep=""))
testUniversities <- testUniversities %>% mutate(`Adjustment` = gsub("SAT/ACT 25th-75th percentile", "Estimated median SAT", `Adjustment`))

testLAs <- testLAs %>% mutate(
  `Size of decline` = rep(NA)
)
testLAs$`Size of decline`[testLAs$`Difference` %in% c(0)] <- "No change"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(1)] <- "1 spot"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(2)] <- "2 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(3)] <- "3 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(4)] <- "4 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(5)] <- "5 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(6)] <- "6 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(7)] <- "7 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(8)] <- "8 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(9)] <- "9 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(10)] <- "10 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(11)] <- "11 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(12, 13)] <- "12-13 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(14, 15, 16)] <- "14-16 spots"
testLAs$`Size of decline`[testLAs$`Difference` %in% c(17, 18, 19, 20)] <- "17-20 spots"
testLAs$`Size of decline`[testLAs$`Difference` > 20] <- "More than 20 spots"
testLAs$`Size of decline` <- factor(testLAs$`Size of decline`) 
testLAs$`Size of decline` <- factor(testLAs$`Size of decline`, levels=c(
  "No change",
  "1 spot",
  "2 spots",
  "3 spots",
  "4 spots",
  "5 spots",
  "6 spots",
  "7 spots",
  "8 spots",
  "9 spots",
  "10 spots",
  "11 spots",
  "12-13 spots",
  "14-16 spots",
  "17-20 spots",
  "More than 20 spots"
)) 
testLAs <- testLAs %>% mutate(`Count` = rep(1))
testLAs <- testLAs %>% mutate(`Adjustment size` = ifelse(
  `Adjusted variable` == "SAT/ACT 25th-75th percentile", round(`Adjustment size`, 0), ifelse(
    `Adjusted variable` == "Median federal loan debt for borrowers", paste("+", dollar(`Adjustment size`, 1), sep=""), ifelse(
      `Adjusted variable` %in% c("Average 6-year graduation rate", "Average first-year student retention rate"), percent(`Adjustment size`, accuracy=0.1), "")
  )
)
)
testLAs <- testLAs %>% mutate(`Adjustment` = paste(`Adjusted variable`, ": ", `Adjustment size`, sep=""))
testLAs <- testLAs %>% mutate(`Adjustment` = gsub("SAT/ACT 25th-75th percentile", "Estimated median SAT", `Adjustment`))

#### End #### 

#### Order adjustments ####

testUniversities$`Adjustment` <- factor(testUniversities$`Adjustment`, levels=c(
  "Estimated median SAT: -20",
  "Estimated median SAT: -40",
  "Estimated median SAT: -60",
  "Estimated median SAT: -80",
  "Estimated median SAT: -100",
  "Estimated median SAT: -120",
  "Estimated median SAT: -140",
  "Estimated median SAT: -160",
  "Estimated median SAT: -180",
  "Estimated median SAT: -200",
  "Average 6-year graduation rate: -5.0%",
  "Average 6-year graduation rate: -10.0%",
  "Average 6-year graduation rate: -15.0%",
  "Median federal loan debt for borrowers: +$2,500",
  "Median federal loan debt for borrowers: +$5,000",
  "Median federal loan debt for borrowers: +$7,500",
  "Median federal loan debt for borrowers: +$10,000",
  "Median federal loan debt for borrowers: +$15,000",
  "Average first-year student retention rate: -5.0%",
  "Average first-year student retention rate: -10.0%",
  "Average first-year student retention rate: -15.0%"
))
testLAs$`Adjustment` <- factor(testLAs$`Adjustment`, levels=c(
  "Estimated median SAT: -20",
  "Estimated median SAT: -40",
  "Estimated median SAT: -60",
  "Estimated median SAT: -80",
  "Estimated median SAT: -100",
  "Estimated median SAT: -120",
  "Estimated median SAT: -140",
  "Estimated median SAT: -160",
  "Estimated median SAT: -180",
  "Estimated median SAT: -200",
  "Average 6-year graduation rate: -5.0%",
  "Average 6-year graduation rate: -10.0%",
  "Average 6-year graduation rate: -15.0%",
  "Median federal loan debt for borrowers: +$2,500",
  "Median federal loan debt for borrowers: +$5,000",
  "Median federal loan debt for borrowers: +$7,500",
  "Median federal loan debt for borrowers: +$10,000",
  "Median federal loan debt for borrowers: +$15,000",
  "Average first-year student retention rate: -5.0%",
  "Average first-year student retention rate: -10.0%",
  "Average first-year student retention rate: -15.0%"
))

#### End #### 

#### Create charts for report ####

fig1 <- aggregate(data=testUniversities, `Count` ~ `Adjustment` + `Size of decline`, FUN=sum)
fig1a <- aggregate(data=testUniversities, `Count` ~ `Adjustment`, FUN=sum) %>% rename(`Total observations` = `Count`)
fig1 <- left_join(x=fig1, y=fig1a, by="Adjustment")
rm(fig1a)
fig1 <- fig1 %>% mutate(`Share of colleges` = `Count` / `Total observations`)
fig1$`Size of decline` <- factor(fig1$`Size of decline`, levels=rev(levels(fig1$`Size of decline`)))
fig1$`Adjustment` <- factor(fig1$`Adjustment`, levels=rev(levels(fig1$`Adjustment`)))
ggplot(data=fig1, mapping=aes(x=`Share of colleges`, y=`Adjustment`, fill=`Size of decline`)) + geom_bar(stat="identity", position="stack", color="lightgray", linewidth=0.1) + scale_x_continuous(labels=percent_format(accuracy=1)) + labs(x="Share of national universities")

fig2 <- aggregate(data=testLAs, `Count` ~ `Adjustment` + `Size of decline`, FUN=sum)
fig2a <- aggregate(data=testLAs, `Count` ~ `Adjustment`, FUN=sum) %>% rename(`Total observations` = `Count`)
fig2 <- left_join(x=fig2, y=fig2a, by="Adjustment")
rm(fig2a)
fig2 <- fig2 %>% mutate(`Share of colleges` = `Count` / `Total observations`)
fig2$`Size of decline` <- factor(fig2$`Size of decline`, levels=rev(levels(fig2$`Size of decline`)))
fig2$`Adjustment` <- factor(fig2$`Adjustment`, levels=rev(levels(fig2$`Adjustment`)))
ggplot(data=fig2, mapping=aes(x=`Share of colleges`, y=`Adjustment`, fill=`Size of decline`)) + geom_bar(stat="identity", position="stack", color="lightgray", linewidth=0.1) + scale_x_continuous(labels=percent_format(accuracy=1)) + labs(x="Share of national liberal arts colleges")

#### End #### 
