#setting up R
library(tidyverse)


#load data
fifa <- read_csv("datasets//fifa21_raw_data_v2.csv")

#creating a duplicate of the data set
fifa_dup <- fifa

#viewing data structure 
glimpse(fifa)
skimr::skim_without_charts(fifa)


#the column names were in proper and upper case and included spaces and special
#characters. To clean the column names:
fifa <- fifa %>% janitor::clean_names()


#separating the position column into position 1, position 2 and position 3
fifa <- separate(fifa, positions, into = c("position_1", "position_2",
                                           "position_3"), sep = ", ")


#cleaning the height column

#checking for the various distinct values in height variable
unique(fifa$height)

#height is measured in cm and ft and included the unit of measurement resulting
#in a character variable instead of numeric variable. To clean the height column

#removing "cm" and "[\"]
fifa$height <- str_remove(fifa$height, 'cm|\"')

#replacing "'" with "." ,converting the height column to numerical variable and 
#converting the measurements in ft to cm
fifa$height <- str_replace(fifa$height, "'", ".") %>% as.numeric() %>% 
  if_else(. < 50, . * 30.48, .) %>% round(digits = 0) 

#renaming  the height column to height(cm)
fifa <- rename(fifa, "height(cm)" = height)



#cleaning the weight column

#checking for the various distinct values in weight variable
unique(fifa$weight)

#weight is measured in lbs and kg, converting the column to a uniform measurement 
#in kg

#removing kg and replacing lbs with a space
fifa$weight <- str_remove(fifa$weight, "kg") %>% str_replace(., "lbs", " ")

#using the space to identify measurement that were made in lbs and converting 
#them to kg
fifa$weight <- if_else(grepl(" ", fifa$weight), as.numeric(fifa$weight) * 0.454,
                       as.numeric(fifa$weight)) %>% round(digits = 0) 

#renaming the weight column to weight(kg)
fifa <- rename(fifa, "weight(kg)" = weight)


#converting the joined column to a date data type
fifa$joined <- lubridate::mdy(fifa$joined)


#removing special characters from sm, wf and ir columns
fifa$w_f <- str_remove(fifa$w_f, '[^[:alnum:] ]') %>% as.numeric
fifa$sm<- str_remove(fifa$sm, '[^[:alnum:] ]') %>% as.numeric
fifa$ir <- str_remove(fifa$ir, '[^[:alnum:] ]') %>% as.numeric



#cleaning the hits column

#checking for the various distinct values in hits variable
unique(fifa$hits)

#converting K to thousands
fifa$hits <- if_else(grepl("K", fifa$hits), 
                     str_remove(fifa$hits, "K") %>% as.numeric(.) * 1000, 
                     as.numeric(fifa$hits))


#cleaning the value column

#removing the €
fifa$value <- str_remove(fifa$value, "€")

#checking for the various distinct values in value variable
unique(fifa$value)

#converting M to millions and K to thousands
fifa$value<- if_else(
              grepl("M", fifa$value), 
              str_remove(fifa$value, "M") %>% as.numeric(.) * 1000000,
              if_else(
                grepl("K", fifa$value),
                str_remove(fifa$value, "K") %>% as.numeric(.) * 1000,
                as.numeric(fifa$value)
              )
            )

#renaming the value column to value(€)
fifa <- rename(fifa, "value(€)" = value)


#cleaning the wage column

#checking for the various distinct values in wage variable
unique(fifa$wage)

#removing the €
fifa$wage <- str_remove(fifa$wage, "€")

#converting M to millions and K to thousands
fifa$wage<- if_else(
    grepl("K", fifa$wage),
    str_remove(fifa$wage, "K") %>% as.numeric(.) * 1000,
    as.numeric(fifa$wage)
  )

#renaming the value column to value(€)
fifa <- rename(fifa, "wage(€)" = wage)


#cleaning the release_clause column

#checking for the various distinct values in release_clause variable
unique(fifa$release_clause)

#removing the €
fifa$release_clause <- str_remove(fifa$release_clause, "€")

#converting M to millions and K to thousands
fifa$release_clause<- if_else(
  grepl("M", fifa$release_clause), 
  str_remove(fifa$release_clause, "M") %>% as.numeric(.) * 1000000,
  if_else(
    grepl("K", fifa$release_clause),
    str_remove(fifa$release_clause, "K") %>% as.numeric(.) * 1000,
    as.numeric(fifa$release_clause)
  )
)

#renaming the value column to value(€)
fifa <- rename(fifa, "release_clause(€)" = release_clause)



# creating a column to indicate players' current contract status
fifa <- fifa %>% mutate(players_current_contract_status = 
                  if_else(grepl("Free", contract), "Free", 
                          if_else(
                            grepl("Loan", contract), "Loan",
                            "Full Contract"
                            )
                          )
                )

#comparing the data set to ensure no data points have been lost during the 
#cleaning process
skimr::skim_without_charts(fifa)
skimr::skim_without_charts(fifa_dup)


#exporting the cleaned dataset
write.csv(fifa, "fifa21_cleaned.csv", row.names = FALSE)























