units <- data.frame(name=c("knife", "glock", "rifle", "sniper", "boss"),
                    hp=c(100,100,100,100,120),
                    mobil=c(10,8,6,3,8),
                    attack=c(5,3,6,8,8),
                    defence=c(0,0,0,0,2),
                    range_min=c(1,1,1,3,1),
                    range_max=c(8,10,16,20,16),
                    price=c(6,7,12,15,20),
                    acc=c(2,4,6,7,6))

units$acc_min <- -1*units$acc + 10
units$acc_max <- 100 - 1*units$acc

save(units, file="data/units.RData")