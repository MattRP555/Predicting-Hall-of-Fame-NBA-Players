#load in the packages
library(xml2)
library(rvest)
library(jsonlite)
library(selectr)
library(stringr)
library(plyr)
library(openxlsx)
#scrapeing from basketball-reference.com
#this will be done to acquire more up to date information

#alphabet list excluding X that will go through the player directory
#by last name
alphabet<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","y","z")

#create an empty data frame for all players
all_player_df<-data.frame()
count<-1

#for loop that will run through the player directory on
#basketball reference to acquire their stats
for (letter_ind in 1:length(alphabet)){
  #first is getting the html script for players with last name starting
  #with current letter
  letter_script<-script_of_letter(alphabet[letter_ind])
  #Now we acquire a list of a vector and dataframe with links in the vector
  #dataframe contains basic information on the player
  list_for_letter_players<-acquiring_player_links(letter_script)
  #next we acquire a list of data frames that have stats about the player
  player_of_letter_data<-get_players_career_stats(list_for_letter_players[[1]])
  #next is making sure to name the columns for different dataframes in the list
  #so that when all dataframes are combined the columns can be distinguished. 
  colnames(player_of_letter_data[[4]])<-str_c(colnames(player_of_letter_data[[4]]),"_reg_adv")
  colnames(player_of_letter_data[[5]])<-str_c(colnames(player_of_letter_data[[5]]),"_playoff")
  colnames(player_of_letter_data[[6]])<-str_c(colnames(player_of_letter_data[[6]]),"_playoff_adv")
  part_of_all_player_df<-cbind(list_for_letter_players[[2]],player_of_letter_data)
  #if statement to make sure to do a special row bind for the first set
  #of players since you can't row bind with empty dataframe
  if (count==1){
    all_player_df<-part_of_all_player_df[FALSE,]
    all_player_df<-rbind.fill(all_player_df,part_of_all_player_df)
  }else{
    all_player_df<-rbind.fill(all_player_df,part_of_all_player_df)
  }
  #count is here to keep track of the program's progress when 
  #it gets timed out. 
  count<-count+1
}
script_of_letter<-function(letter){
  #function that gets the html script for players with last 
  #name starting with letter. this outputs a table with player links
  website<-"https://www.basketball-reference.com/players/"
  if (length(letter)>1){
    stop("letter length was greater than 1")
  }
  #forming the link to get a script of all players 
  #with last name starting with given letter
  letter<-tolower(letter)
  player_url<-str_c(c(website,letter,"/"),collapse = "")
  read_html(player_url)
}

#the link that will be used by the other functions
#to access the website
general_link<-"https://www.basketball-reference.com"
acquiring_player_links<-function(player_index_script){
  #function to get the individual links for each player 
  #in the letter index
  the_table_of_links<-html_node(player_index_script,"#div_players")
  actual_table<-html_node(the_table_of_links,"#players")
  table_basic_player_info<-html_table(actual_table,header=TRUE)
  columns<-html_nodes(the_table_of_links,".left")
  the_links<-html_nodes(columns,"a")
  text_links<-html_attr(the_links,"href")
  ind<-str_detect(text_links,pattern="players")
  text_links<-text_links[which(ind==TRUE)]
  #for loop to form the proper link for players
  for (i in 1:length(text_links)){
    text_links[i]<-str_c(c(general_link,text_links[i]),collapse = "")
  }
  output<-list(text_links,table_basic_player_info)
}
get_players_career_stats<-function(indlink){
  #funciton to retrieve the career stats of each player
  # in the regular season and the playoffs
  #first is initializing dataframes for tables we want
  career_reg_season_totals<-data.frame()
  career_reg_season_advanced<-data.frame()
  career_playoff_totals<-data.frame()
  career_playoffs_advanced<-data.frame()
  player_profile<-data.frame(matrix(ncol=6,nrow = 0))
  x<-c("HOF","MVP","Allstar","Titles","placeofbirth","born")
  colnames(player_profile)<-x
  #for loop that runs through each player's page given by indlink
  #and scrape tables and other basic information from them
  for (player in 1:length(indlink)){
    ind_player_profile<-get_player_profile(indlink[player])
    player_profile<-rbind.fill(player_profile,ind_player_profile)
    reg_season_totals<-extract_table_from_comment(indlink[player],"totals")
    reg_season_advanced<-extract_table_from_comment(indlink[player],"advanced")
    playoff_totals<-extract_table_from_comment(indlink[player],"playoffs_totals")
    playoffs_advanced<-extract_table_from_comment(indlink[player],"playoffs_advanced")
    #all these if else statements are meant to make sure the row bind works if 
    #it is the first player in the list of links. Also, they make sure 
    #that if any tables were not retrieved then NA's are substituted in.
    if (player==1){
      career_reg_season_totals<-reg_season_totals[FALSE,]
    }
    if(as.logical(!is.na(reg_season_advanced))){
      reg_season_advanced<-reg_season_advanced[,-which(colnames(reg_season_advanced)=="")]
      if(player==1){
        career_reg_season_advanced<-reg_season_advanced[FALSE,]
      }
      career_reg_season_advanced<-rbind.fill(career_reg_season_advanced,reg_season_advanced[which(reg_season_advanced$Season=="Career"),])
    }else{
      if(player==1){
        career_reg_season_advanced<-data.frame(NA)
      }
      career_reg_season_advanced<-rbind.fill(career_reg_season_advanced,as.data.frame(NA))
      }
    if(as.logical(!is.na(reg_season_totals))){
      numseasonslst<-reg_season_totals$Season[which(str_detect(reg_season_totals$Season,"[0-9]-[0-9]")==TRUE)]
      seasonsplayed<-length(numseasonslst)
      sum_season<-data.frame(seasons=seasonsplayed)
      if(player==1){
        number_seasons<-data.frame(seasons=0)
        number_seasons[1,]<-sum_season[1,]
      }else{
        number_seasons<-rbind.fill(number_seasons,sum_season)
      }
      career_reg_season_totals<-rbind.fill(career_reg_season_totals,reg_season_totals[which(reg_season_totals$Season=="Career"),])
    }else{
      career_reg_season_totals<-rbind.fill(career_reg_season_totals,as.data.frame(NA))
      number_seasons<-rbind(number_seasons,data.frame(seasons=NA))
    }
    if (as.logical(is.na(playoff_totals))){
      if(player!=1){
      career_playoff_totals<-rbind.fill(career_playoff_totals,as.data.frame(NA))
      }else{
        career_playoff_totals[1,]<-NA
      }
     }else{
       if(player==1){
         career_playoff_totals<-playoff_totals[FALSE,]
       }
      career_playoff_totals<-rbind.fill(career_playoff_totals,playoff_totals[which(playoff_totals$Season=="Career"),])
    }
    if (is.na(playoffs_advanced)){
      if(player!=1){
      career_playoffs_advanced<-rbind.fill(career_playoffs_advanced,as.data.frame(NA))
      }else{
        career_playoffs_advanced[1,]<-NA
      }
    }
    else{
      playoffs_advanced<-playoffs_advanced[,-which(colnames(playoffs_advanced)=="")]
      if(player==1){
      career_playoffs_advanced<-playoffs_advanced[FALSE,]
      }
      career_playoffs_advanced<-rbind.fill(career_playoffs_advanced,playoffs_advanced[which(playoffs_advanced$Season=="Career"),])
    }
  }
  list(player_profile,number_seasons,career_reg_season_totals,career_reg_season_advanced,career_playoff_totals,career_playoffs_advanced)
}
extract_table_from_comment<-function(link,desiredtable){
  #function that reads tables from comments based on how the website script
  #works
  if (!is.character(desiredtable)){
    stop("desiredtable must be a character")
  }
  #this check helps to see if table is in the html script as
  check<-tryCatch(read_html(link),error=function(e){'empty'})
  #if statement is for if data frame was found and converting
  #some of the tables written as comments to proper tables.
  if(check!='empty'){
  information<-read_html(link)
  text_information<-html_text(information)
  chosen_table<-str_c(c('table#',desiredtable),collapse = "")
  script_as_comment<- html_nodes(information,xpath = '//comment()') # select comment nodes
  text_script<-html_text(script_as_comment)    # extract comment text
  smoothed_text_script<-paste(text_script,collapse = '')# collapse to a single string
  if(str_detect(smoothed_text_script,desiredtable)){
   script_html<-read_html(smoothed_text_script)# reparse to HTML
   stat_table<-html_node(script_html,chosen_table)# select the desired table
   html_table(stat_table)}
  else{
    return(data.frame(NA))
  }
  }else{
   return(data.frame(NA))
 }
  
}
get_player_profile<-function(playerlink){
  profile<-data.frame(matrix(ncol=6,nrow = 0))
  colnames(profile) <-c("HOF","MVP","Allstar","Titles","placeofbirth","born")
  check<-tryCatch(read_html(playerlink),error=function(e){'empty'})
  if(check!='empty'){
  player_info<-read_html(playerlink)
  header<-html_node(player_info,"#info")
  header_text<-html_text(header)
  # if statements to detect certain desired features
  # if the info is available then code follows to extract it
  # if it doens't detect then NA is used.
  if(str_detect(header_text,"Born:")){
    header_text_b<-str_replace_all(header_text,pattern = "[\n | \t]",replacement = " ")
    header_text_b<-str_squish(header_text_b)
    header_text_b<-str_split(header_text_b,":")
    ind_b<-lapply(header_text_b,str_detect,pattern="Born")
    ind_b<-(which(ind_b[[1]]==TRUE)+1)
    if(length(ind_b)>=2){ind_b<-ind_b[length(ind_b)]}
    bornline<-header_text_b[[1]][ind_b]
    bornline<-str_remove(bornline,"College")
    birthplace<-NA
    if(str_detect(bornline,"in ")){
      birthplace<-data.frame(birth=bornline)
      birthplace<-separate(birthplace,col = "birth",into=c("before","after"),sep = "in ")
      birthplace<-birthplace$after
      birthplace<-str_remove(birthplace," [a-z][a-z]")
    }else{
      birthplace<-NA
  
    }
    # old code below (not used anymore but kept for reference)
    # birthplace<-str_split(birthplace,pattern = " ")
    # city<-as.character(birthplace[[1]][6])
    # state<-as.character(birthplace[[1]][7])
    # extra<-as.character(birthplace[[1]][8])
    # birthplace<-str_c(city,state,extra,sep=" ")
  }else{
    birthplace<-NA
    bornline<-NA
  }
  if(str_detect(header_text,"Hall of Fame")){
    hof<-1
  }else{
    hof<-0
  }
  if(str_detect(header_text,"All Star")){
    allstar<-1
    accomp<-html_node(header,"#bling")
    accomp_text<-html_text(accomp)
    accomp_text<-str_remove_all(accomp_text," ")
    allstar_text<-str_extract(accomp_text,"([1-9][0-9]|[0-9])xAllStar")
    if(!is.na(allstar_text)){
     allstar<-str_extract(allstar_text,"[1-9][0-9]|[0-9]")
    }
  }else{
    allstar<-0
  }
  if(str_detect(header_text,"NBA Champ")){
    titles<-1
    accomp_t<-html_node(header,"#bling")
    accomp_t_text<-html_text(accomp_t)
    accomp_t_text<-str_remove_all(accomp_t_text," ")
    NBAChamp_text<-str_extract(accomp_t_text,"([1-9][0-9]|[0-9])xNBAChamp")
    if(!is.na(NBAChamp_text)){
      titles<-str_extract(NBAChamp_text,"[1-9][0-9]|[0-9]")
    }
  }else{
    titles<-0
  }
  if(str_detect(header_text,"MVP")){
    mvps<-1
    accomp_mvp<-html_node(header,"#bling")
    accomp_mvp_text<-html_text(accomp_mvp)
    accomp_mvp_text<-str_remove_all(accomp_mvp_text," ")
    mvp_text<-str_extract(accomp_mvp_text,"([1-9][0-9]|[0-9])xMVP")
    if(!is.na(mvp_text)){
      mvps<-str_extract(mvp_text,"[1-9][0-9]|[0-9]")
    }
  }else{
    mvps<-0
  }
  }else{
    hof<-NA
    mvps<-NA
    allstar<-NA
    titles<-NA
    birthplace<-NA
    bornline<-NA
  }
  profile<-data.frame(HOF=hof,MVP=mvps,Allstar=allstar,Titles=titles,placeofbirth=birthplace,born=bornline,stringsAsFactors = FALSE)
  return(profile)

}

#below is a cleanup of uneccesary columns, such as duplicates, before saving
teamrm<-which(str_detect(colnames(all_player_df),"Tm")==TRUE)
all_player_df<-all_player_df[,-teamrm]
seasonrm<-which(str_detect(colnames(all_player_df),"Season")==TRUE)
all_player_df<-all_player_df[,-seasonrm]
NArm<-which(str_detect(colnames(all_player_df),"NA")==TRUE)
all_player_df<-all_player_df[,-NArm]
agerm<-which(str_detect(colnames(all_player_df),"Age")==TRUE)
all_player_df<-all_player_df[,-agerm]
posrm<-which(str_detect(colnames(all_player_df),"Pos_")==TRUE)
all_player_df<-all_player_df[,-posrm]
lgrm<-which(str_detect(colnames(all_player_df),"Lg_")==TRUE)
all_player_df<-all_player_df[,-lgrm]
Grm<-which(str_detect(colnames(all_player_df),"^G_[a-z]*_adv$")==TRUE)
all_player_df<-all_player_df[,-Grm]
MPrm<-which(str_detect(colnames(all_player_df),"^MP_[a-z]*_adv$")==TRUE)
all_player_df<-all_player_df[,-MPrm]
write.csv(all_player_df,file="C:\\Users\\matth\\OneDrive\\Documents\\datasets\\completedatasetV2.csv")
write.xlsx(all_player_df,"C:\\Users\\matth\\OneDrive\\Documents\\datasets\\completedatasetV3.xlsx")
