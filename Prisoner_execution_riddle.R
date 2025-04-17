# "Three prisoners on death row are told that one of them has been chosen 
# at random for execution the following morning, but the other two are to 
# be freed. One privately begs the warden to at least tell him the name of 
# one other prisoner who will be freed, and the warden relents. “Susie will 
# go free,” he says. Suddenly horrified, the first prisoner says that because
# he is now one of only two remaining prisoners at risk, his chances of 
# execution have risen from one-third to one-half!"

library(tictoc)

tic()
set.seed(42)
n <- 1e6
executed <- sample(c("A", "B", "C"), n, replace = TRUE) # 1/3 wahrsch für A,B,C

# Für alle Fälle mit A als Fragender:
named <- character(n)

for (i in 1:n) {
  prisoners <- c("A", "B", "C")
  other <- c("B", "C")
  free_others <- setdiff(other, executed[i])
  named[i] <- sample(free_others, 1)  # Wärter wählt zufällig eine der sicheren Freilassungen
}

# Fälle, in denen Wärter B genannt hat
table(executed[named == "B"]) / sum(named == "B")

# Fälle, in denen Wärter C genannt hat
table(executed[named == "C"]) / sum(named == "C")
toc()