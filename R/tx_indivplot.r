
#####################
# Line drawing
#####################

#add rows for each unit of time (i.e. 1 to ndays)
#y axis is one unit for each pt_id - currently assumes px_id are sequentially numbered and numerical. May need to add code to do this if, e.g. pt_id is non-sequential and string.
#shading represents the different treatments


treats_ind<-treats[rep(row.names(treats),treats$dur),]  #slightly off from the true # days.
treats_ind["days"]<-rep(1:277,100)           #this is a total hack!!also doesn't add up (as noted above).

colors <- colorRampPalette(c("dark blue", "white"))(6)
ggplot(treats_ind, aes(x= days,y = pt_id, fill = tx)) + geom_tile() +theme_bw() +scale_fill_manual(values=colors)


