# Your apartment contains a long hallway with 10,000 light bulbs, numbered 1 through 10,000.
# Originally, all of the light bulbs are off.
# On the first day, you pull the chain on every light bulb, turning them all on.
# On the second day, you pull the chain on every even-numbered light bulb, turning them off.
# On the third day, you pull the chain on every light bulb that’s a multiple of three.
# This continues: on the nth day, you toggle all the light bulbs that are multiples of n.
# 
# After 10,000 days, which light bulbs are on?

# The following is the brute force approach to solve this. Surely there is a better way, so will
# revisit at a later date. For now, just wanted to get the result -- it's an interesting one!

hallLights <- function(days=10000, bulbs=10000) {
  bulbStatus <- rep(0,bulbs)
  for (day in 1:days) {
    whichBulbs <- seq(0,bulbs,by=day)[-1]
    bulbStatus[whichBulbs] <- abs(bulbStatus[whichBulbs]-1)
  }
  which(bulbStatus==1)
}
