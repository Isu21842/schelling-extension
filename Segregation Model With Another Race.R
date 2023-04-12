# ######Schelling Segregation Model######
# 
# ##Purpose##
# Determine whether segregation can be explained by individual motivations to avoid being a local minority. Agents will move around
# a two-dimensional space depending on whether they are a racial minority within their local neighborhood.
#
# A population of agents will be generated; each agent will have a race (black or white) and a position in a 2-dimensional space
#
# Each agent will first calculate how far they are from each other agent in the 2D space. The agent will then identify all agents who reside within a set radius around them. These agents constitute the focal agent's neighborhood.
# The focal agent will then determine the race of all agents in their neighborhood. Finally, the agent will determine whether they are in the minority race in their neighborhood.
# 
# In this stage of the life cycle, the agent makes one simple decision: if the agent is not in the minority then they will stay where they are. If the agent is in the minority, they will move to a new random location.
#
# The model will plot the position of agents before and after the movement phases
#
### NEW ADDITIONS
# Created a third type of agent so there is blue, black, and white agents.
# I added an individual difference variable to each agent that randomly assigns them a 'conservative' level
# which corresponds to the personal threshold they consider before they no longer like the neighborhood -- I kept the 
# 'minority' parameter from the previous model though.
# I also kept track of how many times an agent decided to move out of their neighborhood after each iteration of the 
# model.
# At the end, I analyzed the model to determine whether agents who were higher in 'conservatism' were leaving their
# neighborhood more often than agents who were lower in 'conservatism'.



######Packages######
library(ggplot2)
library(ggstatsplot)
library(gganimate)





######Parameter######

#The total number of agents to generate
popsize<-1000

#The maximum number of times the model will run before stopping
maxsteps<-1000

#Minority Threshold#
#The threshold below which an agent will no longer like their neighborhood
minority<-.5


#Neighborhood Radius#
#The radius of the circle which each agent will describe as "their neighborhood"
radius<-10



#####Functions######

#Agent Generation

agentgenerate<-function(popsize){
  
  # Generate random races for agents
  race<-sample(c("black","white","blue"),popsize,replace=T)
  
  #Assign each agent an individual difference trait of their personal 'minority' threshold
  conserv<-runif(popsize,0.2,0.9)
  
  #Assign each agent a random x position and y position in a 2-D space
  xpos<-runif(popsize,0,100)
  ypos<-runif(popsize,0,100)
  
  # Pre-generate an empty vector to store how many times an agent moved
  movetimes<-rep(0,popsize)
  
  #Compile the agents
  agents<-data.frame(race,xpos,ypos,conserv,movetimes)
  
  #Output the agents
  return(agents)
}


#Movement Decision Function
decidemove<-function(agents,popsize,radius,threshold){
  
  #Calculate the distance between each agent and all other agents
  distances<-as.matrix(dist(agents[,2:3]))
  
  #Create a blank vector to store agents' movement decisions
  movedecision<-rep(NA,popsize)
  
  #Loop through each individual agent
  for(a in 1:popsize){
    
    #For each agent, determine whether they are in agent a's neighborhood
    neighbors<-distances[a,]<=radius
    
    #Determine which agents are of the same race as agent a
    samerace<-agents$race[a]==agents$race
    
    #Have each agent calculate the proportion of neighbors who share their race
    assessment<-sum(neighbors & samerace)/sum(neighbors)
    
    #Have each agent decide whether or not to move based on their minority threshold
    movedecision[a]<-assessment<=agents$conserv[a]
    
  }
  
  #Output movedecision
  return(movedecision)
  
}



######Model Start#####

agents<-agentgenerate(popsize)

#Save agents' start position
start<-agents

###Assess###

steps<-0

while(steps<maxsteps){
 
  #Have each agent decide whether they want to move
  agents$move<-decidemove(agents,popsize,radius,threshold)
  
  #Loop through each individual agent
  for(a in 1:popsize){
    #If agent a has decided to move...
    if(agents$move[a]==1){
      
      #...assign them a new x and y position
      agents$xpos[a]<-runif(1,1,100)
      agents$ypos[a]<-runif(1,1,100)
      
      # and store the fact that they moved
      agents$movetimes[a]<-agents$movetimes[a]+1
    }
  }
  
  #Determine whether any; agents wanted to move this round
  endcheck<-sum(agents$move)==0
  
  #If not, end the loop: if so, increment steps by one
  steps<-ifelse(endcheck==T,maxsteps,steps+1)
}

#Save agents' end position
end<-agents

#Plot agents' start positions
t1<-qplot(xpos,ypos,color=race, data=start,xlab="X",ylab="Y")+
  geom_point(size=2.5)+scale_color_manual(values=c("black","blue","white"))+
  theme(panel.background=element_rect(fill="grey"))

#Plot agents' end positions
t2<-qplot(xpos,ypos,color=race, data=end,xlab="X",ylab="Y")+
  geom_point(size=2.5)+scale_color_manual(values=c("Black","blue","white"))+
  theme(panel.background=element_rect(fill="grey"))

#Is conservatism positively associated with the rate of moving out of a neighborhood?
t3<-ggstatsplot::ggscatterstats(
  data = end,
  x = conserv,
  y = movetimes,
  messages = FALSE
)


