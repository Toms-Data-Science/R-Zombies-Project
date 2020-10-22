#This code allows you to simulate a zombie apocalypse, visualise it live and see how
#the two populations of humans and zombies changes over time when you adjust certain parameters.
#To adjust the parameters, scroll to the bottom of this script

set.seed(100)
#Define Functions:
random_step = function(input_vector,speed){
  dx = 0
  dy = 0
  xsam = c(-1,0,1)
  ysam = c(-1,0,1)
  Psam = c(0,1,1)
  Nsam = c(-1,-1,0)
  Lim = 60
  
  if(input_vector[1] == 0){
    xsam = Psam
  }
  
  if(input_vector[1] == Lim){
    xsam = Nsam
  }
  
  if(input_vector[2] == 0){
    ysam = Psam
  }
  
  if(input_vector[2] == Lim){
    ysam = Nsam
  }
  
  
  
  dx = sample(xsam,1,replace=TRUE)
  dy = sample(ysam,1,replace=TRUE)
  
  
  direction_vector = c(dx,dy)
  speed_vector = direction_vector * speed
  output_vector = input_vector + speed_vector
  
  
  if(output_vector[1]>=Lim){
    output_vector[1]=Lim
  }
  
  if(output_vector[2]>=Lim){
    output_vector[2]=Lim
  }
  
  if(output_vector[1]<=0){
    output_vector[1]=0
  }
  
  if(output_vector[2]<=0){
    output_vector[2]=0
  }
  
  
  
  return(output_vector) 
  
}
Apocalypse = function(S,Z,Vh,Vz,dr,Immune){
  
  #Create a "dead list" and set a natural death rate
  DPop = as.data.frame(matrix(data=NA,nrow = 0,ncol = 0))
  DT = 1/dr
  dc = 0
  
  
  #Generate the two Populations:
  Pop1.x = vector()
  Pop1.y = vector()
  Pop1 = matrix(data = NA, nrow=S, ncol=0)
  h = S/10
  
  x=0
  y=0
  
  for (i in 1:h){
    
    for(i in 1:10){
      y= y+1
      Pop1.y = c(Pop1.y,y)
      Pop1.x = c(Pop1.x,x)
    }
    x=x+1
    y=0
  }
  
  dim(Pop1.x)=c(length(Pop1.x),1)
  dim(Pop1.y)=c(length(Pop1.y),1)
  
  
  Pop1 = cbind(Pop1,Pop1.x)
  Pop1 = cbind(Pop1,Pop1.y)
  Pop2 = Pop1 +20
  
  
  Pop = as.data.frame(rbind(Pop1,Pop2))
  colnames(Pop) = c('X','Y')
  
  #Randomly remove x amount of people from the population and place them in an immunity list
  
  ImPop = as.data.frame(Pop[sample(nrow(Pop),Immune),])
  ImLen = nrow(ImPop)

  
  for(i in 1:ImLen){
    HImmune = subset(Pop, subset = X== ImPop[i,1]&Y== ImPop[i,2])
    HSucept = subset(Pop, subset = X!= ImPop[i,1]|Y!= ImPop[i,2])
    Pop = HSucept
  }
  

  #Add A Zombie to Population 1 at a random coordinate
  ZPop = matrix(data=NA, nrow=0, ncol = 2)
  
  for(i in 1:Z){
    SamX = sample(Pop1[,1],1,replace=FALSE)
    SamY = sample(Pop1[,2],1,replace=FALSE)
    ZPop = rbind(ZPop,c(SamX,SamY))
  }
  
  ZPop = as.data.frame(ZPop)
  colnames(ZPop) = c('X','Y')
  
  
  #Make the Zombie and the Population Do A Random Walk
  len= nrow(Pop)
  zlen= nrow(ZPop)
  Steps = 100000
  s=1:Steps
  l=1:len
  zl=1:zlen
  sc = 0
  pv = vector()
  zv = vector()
  sv = vector()
  
  for(i in s){
    len= nrow(Pop)
    zlen= nrow(ZPop)
    Ilen= nrow(ImPop)
    l=1:len
    zl=1:zlen
    Il= 1:Ilen
    
    #Move each human in a random direction
     for (i in l){
      PopStep = random_step(as.vector(Pop[i,]),Vh)
      Pop[i,] = PopStep
    }
    
    for (i in Il){
      ImPopStep = random_step(as.vector(ImPop[i,]),Vh)
      ImPop[i,] = ImPopStep
    }
    
    #Move each zombie in a random direction
    for(i in zl){
      ZombieStep = random_step(as.vector(ZPop[i,]),Vz)
      ZPop[i,]=ZombieStep
    }
    
    #Check each zombie's coordinates and see if they are the same as a human's
  
      for(i in zl){
      Bite = subset(Pop, subset = X== ZPop[i,1]&Y== ZPop[i,2])
      NBite = subset(Pop, subset = X!= ZPop[i,1]|Y!= ZPop[i,2] )
      ZPop = rbind.data.frame(ZPop, Bite)
      Pop = NBite
    }
    
    
    #Step counter
    sc = sc+1
    pv = c(pv,nrow(Pop))
    zv = c(zv,nrow(ZPop))
    sv = c(sv,sc)
    
    
    #Add to the death timer and if threshold met, randomly remove a member from human or zombie population and place them in death list
    dc=dc+1
    if (dc>=DT){
      
      TL = nrow(Pop)+nrow(ZPop)+ nrow(ImPop)
      THpop = (nrow(Pop)+nrow(ImPop))
      TLV = (1:TL)
      select = sample(TLV,1,replace = TRUE)
      
      if(select>THpop){
        zd = ZPop[sample(nrow(ZPop),1),]
        DPop = rbind(DPop,zd)
      }else if(select>nrow(ImPop)){
        hd = Pop[sample(nrow(Pop),1),]
        DPop = rbind(DPop,hd)
      }else{
        Id = ImPop[sample(nrow(ImPop),1),]
        DPop = rbind(DPop,Id)
      }

      DL = nrow(DPop)
      D = 1:DL
      
      for(i in D){
        HDeath = subset(Pop, subset = X== DPop[i,1]&Y== DPop[i,2])
        HLive = subset(Pop, subset = X!= DPop[i,1]|Y!= DPop[i,2])
        Pop = HLive
        
        ZDeath = subset(ZPop, subset = X== DPop[i,1]&Y== DPop[i,2])
        ZLive = subset(ZPop, subset = X!= DPop[i,1]|Y!= DPop[i,2])
        ZPop = ZLive
      }
      dc=0

    }

    #Check if population is overcome yet
    
    
    if (nrow(Pop) == 0){
      break
    }else{
      
    }
    
    #plot out a live 2-d visualisation of the outbreak at each step
    
    plot(ImPop,type = "p", col = "blue", pch=18,xlim = c(0,60),ylim = c(0,60))
    points(ZPop, type = "p", col = "red", pch =  16)
    points(Pop, type = "p", col = "blue", pch =  1)
    if(nrow(DPop)>0){ points(DPop, type = "p", col = "black", pch =  17)}
    
  }
  
  plot(sv,pv,type = "l",col = "blue" , main = paste("Human (Blue) and Zombie (Red) Population Sizes. S=",S,"Z=",Z,"Vh=",Vh,"Vz=",Vz,"Dr =",dr,"Im=",Immune), xlab = "Step", ylab = "Total Population Size")
  points(sv,zv,type = "l", col = "red")
  return(sc)
}

#S is the starting number of humans in each population (must be divisible by 10)
S=200
#z is the number of zombies at the beginning (in total, not per population)
z=1
#Vh is the speed of the humans
Vh=1
#Vz is the speed of the zombies
Vz=1
#Dr is the natural death rate of the population
Dr=0.025
#Imm is the number of immune humans across the populations
Imm=3

A = Apocalypse(S,z,Vh,Vz,Dr,Imm)
A #outputs steps taken to overcome suceptible population
