# Nous calculons la liste des profils pour lesquels DA restitue à la fois en période 1 et 2.

# For that, we create a comparative table, to compare price from one period to another without the filtering bias (uniquely the lines
# which are both in 2 periods).


Old_Table <- crawling_all[crawling_all$period %in% lp,]


Comparative_Table = merge(New_Table, Old_Table, by=c('profilID', 'insurer', 'coverage'), all.x = TRUE, all.y = TRUE)

Comparative_Table = na.omit(Comparative_Table)

Comparative_Table_save <- Comparative_Table
profils_ranking_DA <- Comparative_Table_save$profilID[Comparative_Table_save$insurer == 'Groupe AXA'] 
profils_ranking_DA <- unique(profils_ranking_DA)

#




### Global Ranking ###

New_Table_DA = New_Table[New_Table$profilID %in% profils_ranking_DA,] # If we want to analyse the ranking just for the profils for 
# which Direct Assurance restitue for the 2 periods.
New_Table_DA = New_Table_DA [New_Table_DA$insurer %in% MAINPlayers,]


ranking_top1 = top_propor_Generic(New_Table,top = 1)

ranking_top1$proportion=ranking_top1$cumsum/ranking_top1$cumsum2
ranking_top1$proportion=round(ranking_top1$proportion*100)
ranking_top1 = ranking_top1[,-c(1,4,5,6,8,10,11,12,13)]
ranking_top1 <- as.data.table(ranking_top1)
ranking_top1 <- unique(ranking_top1, by=c("insurer", "coverage"))


save(ranking_top1,file= ("./Tables/ranking_top1.RData")) 




ranking_top3 = top_propor_Generic(New_Table,top = 3)

ranking_top3$proportion=ranking_top3$cumsum/ranking_top3$cumsum2
ranking_top3$proportion=round(ranking_top3$proportion*100)
ranking_top3 = ranking_top3[,-c(1,4,5,6,8,10,11,12,13)]
ranking_top3 <- as.data.table(ranking_top3)
ranking_top3 <- unique(ranking_top3, by=c("insurer", "coverage"))

save(ranking_top3,file= ("./Tables/ranking_top3.RData")) 







### Ranking by player ###

# ranking_player <- genrankovermonths(New_Table, "AXA", "AXA", formulaNames)




ranking_by_player_all <- NULL

for (insurer in All) {
  tryCatch({
    nam <- paste("ranking_player_", insurer, sep = "")
    rank <- genrankovermonths(New_Table, insurer, insurer, formulaNames)
    rank$insurer <- insurer
    assign(nam, rank)
    ranking_by_player_all <- rbind(ranking_by_player_all, rank) 
  }, error=function(e){})
} 

ranking_by_player_all <- ranking_by_player_all[,-c(5,6)]


save(ranking_by_player_all,file= ("./Tables/ranking_by_player_all.RData")) 







### Ranking evolution over periods ###

crawling_all_butalternatifs <- crawling_all[!crawling_all$insurer %in% ALTERNATIFSPlayers,]


ranking_evol_top1 = top_propor_Generic(crawling_all_butalternatifs,top = 1)

ranking_evol_top1$proportion=ranking_evol_top1$cumsum/ranking_evol_top1$cumsum2
ranking_evol_top1$proportion=round(ranking_evol_top1$proportion*100)
ranking_evol_top1 = ranking_evol_top1[,-c(7,8,10,11,12)]
ranking_evol_top1 <- as.data.table(ranking_evol_top1)
ranking_evol_top1 <- unique(ranking_evol_top1, by=c("insurer", "coverage", "period"))
ranking_evol_top1 <- ranking_evol_top1[order(ranking_evol_top1$period),]


ranking_evol_top1_p1 <- ranking_evol_top1[ranking_evol_top1$period==lp,]
ranking_evol_top1_p2 <- ranking_evol_top1[ranking_evol_top1$period==ap,]


ranking_evol_top1 <- merge(ranking_evol_top1_p1,ranking_evol_top1_p2, by=c("insurer", "coverage"), all.x=TRUE, all.y=TRUE)
ranking_evol_top1 = ranking_evol_top1[,-c(5,6,11,12)]

ranking_evol_top1$var_prop <- round((ranking_evol_top1$proportion.y / ranking_evol_top1$proportion.x) - 1,1)

save(ranking_evol_top1,file= ("./Tables/ranking_evol_top1_butalternatifs.RData")) 

write.csv(ranking_evol_top1, "./Tables/ranking_evol_top1_classics.csv")





ranking_evol_top3 = top_propor_Generic(crawling_all_butalternatifs,top = 3)

ranking_evol_top3$proportion=ranking_evol_top3$cumsum/ranking_evol_top3$cumsum2
ranking_evol_top3$proportion=round(ranking_evol_top3$proportion*100)
ranking_evol_top3 = ranking_evol_top3[,-c(7,8,10,11,12)]
ranking_evol_top3 <- as.data.table(ranking_evol_top3)
ranking_evol_top3 <- unique(ranking_evol_top3, by=c("insurer", "coverage", "period"))
ranking_evol_top3 <- ranking_evol_top3[order(ranking_evol_top3$period),]



ranking_evol_top3_p1 <- ranking_evol_top3[ranking_evol_top3$period==lp,]
ranking_evol_top3_p2 <- ranking_evol_top3[ranking_evol_top3$period==ap,]


ranking_evol_top3 <- merge(ranking_evol_top3_p1,ranking_evol_top3_p2, by=c("insurer", "coverage"))
ranking_evol_top3 = ranking_evol_top3[,-c(5,6,11,12)]

ranking_evol_top3$var_prop <- round((ranking_evol_top3$proportion.y / ranking_evol_top3$proportion.x) - 1,1)

save(ranking_evol_top3,file= ("./Tables/ranking_evol_top3.RData")) 

write.csv(ranking_evol_top3, "./Tables/ranking_evol_top3_classics.csv")









### Ranking comparaison over periods (commun profils between 2 periods) uniquely on profiles where DA restitutes ###

crawling_all_butalternatifs <- crawling_all[!crawling_all$insurer %in% ALTERNATIFSPlayers,]







crawling_all_classic_DA <- crawling_all_butalternatifs[crawling_all_butalternatifs$profilID %in% profils_ranking_DA,]

ranking_evol_top1 = top_propor_Generic(crawling_all_classic_DA,top = 1)

ranking_evol_top1$proportion=ranking_evol_top1$cumsum/ranking_evol_top1$cumsum2
ranking_evol_top1$proportion=round(ranking_evol_top1$proportion*100)
ranking_evol_top1 = ranking_evol_top1[,-c(7,8,10,11,12)]
ranking_evol_top1 <- as.data.table(ranking_evol_top1)
ranking_evol_top1 <- unique(ranking_evol_top1, by=c("insurer", "coverage", "period"))
ranking_evol_top1 <- ranking_evol_top1[order(ranking_evol_top1$period),]


ranking_evol_top1_p1 <- ranking_evol_top1[ranking_evol_top1$period==lp,]
ranking_evol_top1_p2 <- ranking_evol_top1[ranking_evol_top1$period==ap,]


ranking_evol_top1 <- merge(ranking_evol_top1_p1,ranking_evol_top1_p2, by=c("insurer", "coverage"), all.x=TRUE, all.y=TRUE)
ranking_evol_top1 = ranking_evol_top1[,-c(5,6,11,12)]

ranking_evol_top1$var_prop <- round((ranking_evol_top1$proportion.y / ranking_evol_top1$proportion.x) - 1,1)


write.csv(ranking_evol_top1, "./Tables/ranking_evol_top1_classics_DA.csv")





ranking_evol_top3 = top_propor_Generic(crawling_all_classic_DA,top = 3)

ranking_evol_top3$proportion=ranking_evol_top3$cumsum/ranking_evol_top3$cumsum2
ranking_evol_top3$proportion=round(ranking_evol_top3$proportion*100)
ranking_evol_top3 = ranking_evol_top3[,-c(7,8,10,11,12)]
ranking_evol_top3 <- as.data.table(ranking_evol_top3)
ranking_evol_top3 <- unique(ranking_evol_top3, by=c("insurer", "coverage", "period"))
ranking_evol_top3 <- ranking_evol_top3[order(ranking_evol_top3$period),]



ranking_evol_top3_p1 <- ranking_evol_top3[ranking_evol_top3$period==lp,]
ranking_evol_top3_p2 <- ranking_evol_top3[ranking_evol_top3$period==ap,]


ranking_evol_top3 <- merge(ranking_evol_top3_p1,ranking_evol_top3_p2, by=c("insurer", "coverage"))
ranking_evol_top3 = ranking_evol_top3[,-c(5,6,11,12)]

ranking_evol_top3$var_prop <- round((ranking_evol_top3$proportion.y / ranking_evol_top3$proportion.x) - 1,1)


write.csv(ranking_evol_top3, "./Tables/ranking_evol_top3_classics_DA.csv")


