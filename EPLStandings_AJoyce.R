#EPL Standings Assignment
#Abbie Joyce

#libraries
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
  
input_date <- (Date="04/25/2020")
input_date <- (Season= "2019/20")


# Main Function -----------------------------------------------------------

EPL_Standings <- function(Date, Season) {
      # Step 1: Use season inputs to download the correct file
    season2122 <- ("https://www.football-data.co.uk/mmz4281/2122/E0.csv")
    season2021 <- ("https://www.football-data.co.uk/mmz4281/2021/E0.csv")
    season1920 <- ("https://www.football-data.co.uk/mmz4281/1920/E0.csv")
    
    
    data <- if(Date <= 25/07/2020) {
      read_csv(url(season1920))
    } else if(Date > 12/9/2020 & Date <= 23/05/2021) {
      read_csv(url(season2021))
    } else {
      read_csv(url(season2021))
    }
    
    # Step 2: Select the appropriate columns
    data <- data %>%
      select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>%
    
      # Step 3: Use the Date input to filter the data set
        #convert date column in our data frame to a universal format
        
      mutate(Date, format= "%m/%d%y") %>%
        #filter data frame based on date input
      filter(Date <= mdy(`Date`))
     
    # Step 4: Split home and away and stack on each other
    home <- data %>%
      mutate(HWin  = ifelse (FTR == 'H' & FTHG > FTAG, 1, 0),
             HDraw = ifelse (FTR == 'D' & FTHG == FTAG, 1, 0),
             HLose = ifelse (FTR == 'A' & FTHG < FTAG, 1, 0)) %>%
      group_by(HomeTeam) %>% 
      summarise(HPlyd = length(HomeTeam),
                HWin  = sum(HWin),
                HDraw = sum(HDraw),
                HLose = sum(HLose),
                HFor  = sum(FTHG), 
                HAg   = sum(FTAG)) %>%
      rename(TeamName = HomeTeam) 
    
    away <- data %>%
      mutate(AWin  = ifelse (FTR == 'A' & FTAG > FTHG, 1, 0),
             ADraw = ifelse (FTR == 'D' & FTAG == FTHG, 1, 0),
             ALose = ifelse (FTR == 'H' & FTAG < FTHG, 1, 0)) %>%
      group_by(AwayTeam) %>% 
      summarise(APlyd = length(AwayTeam),
                AWin  = sum(AWin),
                ADraw = sum(ADraw),
                ALose = sum(ALose),
                AFor  = sum(FTAG), 
                AAg   = sum(FTHG)) %>%
      rename(TeamName = AwayTeam) 
      
    final_data <- full_join(home, away, by="TeamName")
    
    # Step 5: Calculate the variables specified in the write up
    final_standings <- final_data %>%
      mutate(Record = str_c((HWin+AWin), "-", (HLose+ALose), "-", (HDraw+ADraw)),
             HomeRec = str_c(HWin, "-", HLose, "-", HDraw),
             AwayRec = str_c(AWin, "-", ALose, "-", ADraw),
             MatchesPlayed = HPlyd+APlyd,
             Points = ((HWin+AWin)*3)+sum(HDraw+ADraw),
             PPM = Points/MatchesPlayed,
             PtPct = (Points / 3) * MatchesPlayed,
             GS = HFor + AFor,
             GSM = GS/MatchesPlayed,
             GA = HAg + AAg,
             GAM = GA/MatchesPlayed) %>%
      select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM) %>%
    
   # Last main step is to do the following 
      #standings in descending order
      arrange(desc(PPM), desc(GSM), GAM) 
    
      # Final: return the final data frame
    return(final_standings)
}


EPL_Standings("04/25/2020", "2019/20")
