 library("devtools")
 install_github("hiR", "hinstitute")
 library("hiR")
 #We're going to scrape data from twittercounter.com to
 #compare klout scores of twitter users with:
 #the highest number of followers
 #the highest number of friends
 #the highest number of tweets

 #STEP ONE: generate data.frame of urls and types
 subpages <- seq(0, 80, 20)
 follower_base <- 'http://twittercounter.com/pages/100'
 follower_urls <- paste0(follower_base, "/", subpages)
 follower_df <- data.frame(url = follower_urls,
                           list = rep("followers",  length(follower_urls)),
                           subpage = subpages,
                           stringsAsFactors = F
                           )

 friend_base <- 'http://twittercounter.com/pages/friends'
 friend_urls <- paste0(friend_base, "/", subpages)
 friend_df <- data.frame(url = friend_urls,
                         list = rep("friends", length(friend_urls)),
                         subpage = subpages,
                         stringsAsFactors = F)

 tweet_base <- 'http://twittercounter.com/pages/tweets'
 tweet_urls <- paste0(tweet_base, "/", subpages)
 tweet_df <- data.frame(url = tweet_urls,
                        list = rep("tweets", length(friend_urls)),
                        subpage = subpages,
                        stringsAsFactors = F)

 df <- rbind(follower_df, friend_df, tweet_df)

 #STEP TWO: Scrape data
 #create scraping function
 getHandles <- function(df) {
     #download page
     library("RCurl")
     url <- as.character(df$url)
     page <- getURL(url)
     library("XML")
     tree <- htmlTreeParse(page, useInternalNodes=T)

     #get handles
     handle_nodes <- getNodeSet(tree, '//*[@class="row100user"]/div/a')
     handle <- laply(handle_nodes, function(x) {
                     handles <- xmlGetAttr(x, "href")
                     gsub("/", "", handles)
         })

    #calculate rank
     start <- as.numeric(df$subpage)
     n_handle <- length(handle)
     n1 <- start + 1
     n2 <-  start + n_handle
     rank <- n1:n2

     #list
     list <- rep(df$list, n_handle)

     #return
     data.frame(handle, rank, list, stringsAsFactors=F)
 }

 #Run scraping function
 library("plyr")
 twitter_counter <- ddply(df, .(url), getHandles, .progress="text")
 twitter_counter <- twitter_counter[,-1]

 #STEP THREE: Get Klout scores
 library("hiR")
 klout_data <- get_klout_scores(twitter_handles=twitter_counter$handle, api_key="8yng356gnjg37cvn4esbtewy", na_omit=FALSE)
 df <- data.frame(twitter_counter, klout_data)

 #STEP FOUR: plot comparative distributions
 #subsets and colors
 lists <- unique(df$list)
 lists <- data.frame(l = lists, stringsAsFactors=F)
 library("scales")
 cols <- c("#5f0000", "#005e5f", "#005f30")
 lists$col <- alpha(cols, 0.3)

 #parameters
 par(family="serif",
    xaxs="i",
    cex.axis=0.7,
    mai=c(0.8,0.8,0.5,0.3),
    col.axis="grey50",
    lend="round",
    bty="n"
    )
 #shell
 dummy_x <- as.numeric(df$score[df$list=="followers"])
 plot(density(dummy_x),
     type="n",
     xlim=c(0,105),
     xlab="Klout Score",
     main="Density of Klout Scores by Top 100 lists"
     )
 #data
 for(i in 1:nrow(lists)) {
    to_plot <- df[df$list==lists$l[i],]
    polygon(density(na.omit(as.numeric(to_plot$score))), col=lists$col[i])
 }
 #legend
 legend(x=0, y=0.08, legend=paste("most", lists$l), col=lists$col, pch=20, bty="n")
