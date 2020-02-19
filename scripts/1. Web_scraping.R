# Source the functions created in Function.R
source("scripts/Functions.R")


# Scrape fanwork id first; plug into the url

# Scrape the id for each fanwork
## The urls for all the pages of the fiction list
urls <- c("https://archiveofourown.org/tags/Sherlock%20(TV)/works?commit=Sort+and+Filter&include_work_search%5Bfreeform_ids%5D%5B%5D=968&page=1&utf8=%E2%9C%93&work_search%5Bcomplete%5D=&work_search%5Bcrossover%5D=&work_search%5Bdate_from%5D=&work_search%5Bdate_to%5D=&work_search%5Bexcluded_tag_names%5D=&work_search%5Blanguage_id%5D=en&work_search%5Bother_tag_names%5D=&work_search%5Bquery%5D=&work_search%5Bsort_column%5D=revised_at&work_search%5Bwords_from%5D=&work_search%5Bwords_to%5D=",
          paste0("https://archiveofourown.org/tags/Sherlock%20(TV)/works?commit=Sort+and+Filter&include_work_search%5Bfreeform_ids%5D%5B%5D=968&page=", 2:795, "&utf8=%E2%9C%93&work_search%5Bcomplete%5D=&work_search%5Bcrossover%5D=&work_search%5Bdate_from%5D=&work_search%5Bdate_to%5D=&work_search%5Bexcluded_tag_names%5D=&work_search%5Blanguage_id%5D=en&work_search%5Bother_tag_names%5D=&work_search%5Bquery%5D=&work_search%5Bsort_column%5D=revised_at&work_search%5Bwords_from%5D=&work_search%5Bwords_to%5D="))


# Get the id for each fanwork
# id scraped on 11/18/2019

## Scrape page 1-200
id1 <- read_html(urls[1]) %>%
  html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
  html_attr(name = "id") %>%
  substring(6) %>%
  as.numeric()

for (i in 2:200) {
  
  ## Add a time delay 
  Sys.sleep(5)
  
  id.temp <- read_html(urls[i]) %>%
    html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
    html_attr(name = "id") %>%
    substring(6) %>%
    as.numeric()
  
  id1  <- c(id1, id.temp)
}
save(id1, file="data/id1.RDATA")


## Scrape page 201-400
id2 <- read_html(urls[201]) %>%
  html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
  html_attr(name = "id") %>%
  substring(6) %>%
  as.numeric()

for (i in 202:400) {
  
  ## Add a time delay 
  Sys.sleep(5)
  
  id.temp <- read_html(urls[i]) %>%
    html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
    html_attr(name = "id") %>%
    substring(6) %>%
    as.numeric()
  
  id2  <- c(id2, id.temp)
}

save(id2,file="data/id2.RDATA")


## Scrape page 401-600
id3 <- read_html(urls[401]) %>%
  html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
  html_attr(name = "id") %>%
  substring(6) %>%
  as.numeric()

for (i in 402:600) {
  
  ## Add a time delay 
  Sys.sleep(5)
  
  id.temp <- read_html(urls[i]) %>%
    html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
    html_attr(name = "id") %>%
    substring(6) %>%
    as.numeric()
  
  id3  <- c(id3, id.temp)
}

## Scrape page 601-795
id4 <- read_html(urls[601]) %>%
  html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
  html_attr(name = "id") %>%
  substring(6) %>%
  as.numeric()

for (i in 602:795) {
  
  ## Add a time delay 
  Sys.sleep(5)
  
  id.temp <- read_html(urls[i]) %>%
    html_nodes(xpath = "//ol[@class='work index group']/li[@class='work blurb group']") %>%
    html_attr(name = "id") %>%
    substring(6) %>%
    as.numeric()
  
  id4  <- c(id4, id.temp)
}

save(id4, file = "data/id4.RDATA")


# Combine all the ids
#load("data/id1.RDATA")
#load("data/id2.RDATA")
#load("data/id2_5.RDATA")
#load("data/id3.RDATA")
#load("data/id4.RDATA")
id <- c(id1, id2, id2.5, id3, id4)

# Save the IDs
save(id, file = "data/id.RDATA")



# Scrape information of each fanfiction (each id)
# The get_alldat function is defined in Functions.R
# Each of us scraped part of the data on different days because it would take too long to scrape all the data at once.

load("data/id.RDATA")

## fanfic 1-1000
## Scrape on 11/20/2019
p1 <- 1
p2 <- 1000
fandata1 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata1 <- rbind(fandata1, dat.temp)
}
save(fandata1, file = "data/fandata1.RDATA")


## fanfic 1001-2000
## Scrape on 11/20/2019
p1 <- 1001
p2 <- 2000
fandata2 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata2 <- rbind(fandata2, dat.temp)
}

save(fandata2, file = "data/fandata2.RDATA")


## fanfic 2001-3000
## Scrape on 11/20/2019
p1 <- 2001
p2 <- 3000 
fandata3 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata3 <- rbind(fandata3, dat.temp)
}

save(fandata3, file = "data/fandata3.RDATA")


## fanfic 3001-4000   
## Scrape on 11/20/2019
p1 <- 3001
p2 <- 4000
fandata4 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata4 <- rbind(fandata4, dat.temp)
}

save(fandata4, file = "data/fandata4.RDATA")


## fanfic 4001-5000
## Scrape on 11/20/2019
p1 <- 4001
p2 <- 5000
fandata5 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata5 <- rbind(fandata5, dat.temp)
}

save(fandata5, file = "data/fandata5.RDATA")


## fanfic 5001-6000
## Scrape on 11/20/2019
p1 <- 5001
p2 <- 6000
fandata6 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata6 <- rbind(fandata6, dat.temp)
}

save(fandata6, file = "data/fandata6.RDATA")


## fanfic 6001-7000
## Scrape on 11/27/2019
p1 <- 6001
p2 <- 7000
fandata7 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata7 <- rbind(fandata7, dat.temp)
}

save(fandata7, file = "data/fandata7.RDATA")


## fanfic 7001-8000
## Scrape on 11/20/2019
p1 <- 7001
p2 <- 8000
fandata8 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata8 <- rbind(fandata8, dat.temp)
}

save(fandata8, file = "data/fandata8.RDATA")


## fanfic 8001-9000
## Scrape on 11/20/2019
p1 <- 8001
p2 <- 9000
fandata9 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata9 <- rbind(fandata9, dat.temp)
}

save(fandata9, file = "data/fandata9.RDATA")


## fanfic 9001-10000
## Scrape on 11/20/2019
p1 <- 9001
p2 <- 10000
fandata10 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata10 <- rbind(fandata10, dat.temp)
}

save(fandata10, file = "data/fandata10.RDATA")


## fanfic 10001-11000
## Scrape on 11/18/2019
p1 <- 10001
p2 <- 11000
fandata11 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata11 <- rbind(fandata11, dat.temp)
}

save(fandata11, file = "data/fandata11.RDATA")


## fanfic 11001-12000
## Scrape on 11/18/2019
p1 <- 11001
p2 <- 12000
fandata12 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata12 <- rbind(fandata12, dat.temp)
}

save(fandata12, file = "data/fandata12.RDATA")


## fanfic 12001-13000
## Scrape on 11/18/2019
p1 <- 12001
p2 <- 13000
fandata13 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata13 <- rbind(fandata13, dat.temp)
}

save(fandata13, file = "data/fandata13.RDATA")


## fanfic 13001-14000
## Scrape on 11/19/2019
p1 <- 13001
p2 <- 14000
fandata14 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata14 <- rbind(fandata14, dat.temp)
}

save(fandata14, file = "data/fandata14.RDATA")


## fanfic 14001-15000
## Scrape on 11/19/2019
p1 <- 14001
p2 <- 15000
fandata15 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata15 <- rbind(fandata15, dat.temp)
}

save(fandata15, file = "data/fandata15.RDATA")


## fanfic 15001-16000
## Scrape on 11/19/2019
p1 <- 15001
p2 <- length(id)
fandata16 <- get_alldat(id[p1])
for (i in (p1+1):p2) {
  
  ## Add a time delay
  Sys.sleep(5)
  
  dat.temp <- get_alldat(id[i])
  fandata16 <- rbind(fandata16, dat.temp)
}

save(fandata16, file = "data/fandata16.RDATA")



# Merge all the fandata
load("data/fandata1.RDATA")
load("data/fandata2.RDATA")
load("data/fandata3.RDATA")
load("data/fandata4.RDATA")
load("data/fandata5.RDATA")
load("data/fandata6.RDATA")
load("data/fandata7.RDATA")
load("data/fandata8.RDATA")
load("data/fandata9.RDATA")
load("data/fandata10.RDATA")
load("data/fandata11.RDATA")
load("data/fandata12.RDATA")
load("data/fandata13.RDATA")
load("data/fandata14.RDATA")
load("data/fandata15.RDATA")
load("data/fandata16.RDATA")

fandata <- rbind(fandata1, fandata2, fandata3, fandata4, fandata5, fandata6, fandata7, fandata8, fandata9, fandata10, fandata11, fandata12, fandata13, fandata14, fandata15, fandata16)

# Add scraping date
fandata <- fandata %>%
  mutate(scrape_date = c(rep(ymd("2019-11-20"), 6000),
                         rep(ymd("2019-11-27"), 1000),
                         rep(ymd("2019-11-20"), 3000),
                         rep(ymd("2019-11-18"), 3000),
                         rep(ymd("2019-11-19"), 2899)))

save(fandata, file = "data/fandata.RDATA")


# Create a sample to test the code
#fandata_sample <- fandata[sample(1:nrow(fandata), 1000), ]
#save(fandata_sample, file = "data/fandata_sample.RDATA")

