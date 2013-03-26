
skill1 <- c("High","Med","Low")
skill2 <- c("High","Med","Low")
troph <- c("Gold","Silver","None")

calcDGRFrame(list(skill1),troph,list(log(.5),log(1)),list(1,-1))
calcDGRFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,-1))
calcDGRFrame(list(skill1),troph,list(log(.5),log(.5)),list(1,1))


calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(1,-1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(-1,-1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,-1))
calcDPCFrame(list(skill1),troph,list(log(.01),log(.5)),list(0,-1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(.5)),list(1,1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,0))


cbind(expand.grid(1:3,1:3),
      theta=OffsetConjunctive(expand.grid(1:3,1:3),-1,c(0,.5)))
cbind(expand.grid(1:3,1:3),
      theta=OffsetDisjunctive(expand.grid(1:3,1:3),-1,c(0,.5)))

prior <- calcDPCTable(list(skill1),troph,list(log(.5),log(.5)),list(1,-1))
truedist <- calcDPCTable(list(skill1),troph,list(log(1),log(.25)),list(2,-.5))

post1 <- prior + round(1000*truedist)
map1 <- mapDPC(post1,list(skill1),troph,list(log(.5),log(.5)),list(1,-1))
