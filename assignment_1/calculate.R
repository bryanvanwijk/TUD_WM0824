Sys.setenv('R_MAX_VSIZE'=32000000000)
Sys.setenv(TZ="Europe/Amsterdam")
options(repos="https://cloud.r-project.org/")
install.packages("tidyverse")
install.packages("ggplot2")
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

df <- read.csv("poodlessl.csv")

# summary(df) 
# names(df) # prints the column names

# Convert string timestamp to date object
df$posix_date <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%OS")

# Round every date to the first day of the week
df$date_round_week <- floor_date(df$posix_date, "week")

# Round every date to a day
df$date_round_day <- floor_date(df$posix_date, "day")

# Per day the number of unique IP
count_unique_per_day <- ddply(df, .(date_round_day), summarize,  count = length(unique(ip)))

# Per day the number of SSL3 usages 
count_handshake_day <- ddply(df, .(date_round_day), summarize, count=length(handshake), count_not_ssl3 = sum(handshake != "SSLv3"), count_ssl3 = sum(handshake == "SSLv3"), relative_ssl3 = sum(handshake == "SSLv3")/length(handshake))

# Per week the number of SSL3 usages 
count_handshake_week <- ddply(df, .(date_round_week), summarize, count=length(handshake), count_not_ssl3 = sum(handshake != "SSLv3"), count_ssl3 = sum(handshake == "SSLv3"), relative_ssl3 = sum(handshake == "SSLv3")/length(handshake))

# Group data per IP
df_group_by_ip <- df %>% group_by(ip)
ip_with_mult_entries <- df_group_by_ip %>% filter(n() > 1)

first_entry_per_ip <- df_group_by_ip %>% filter(posix_date == min(posix_date))
first_entry_per_ip_mult <- ip_with_mult_entries %>% filter(posix_date == min(posix_date))
last_entry_per_ip_mult <- ip_with_mult_entries %>% filter(posix_date == max(posix_date))

initial_insecure_servers <- first_entry_per_ip_mult[first_entry_per_ip_mult$handshake == "SSLv3",]
final_insecure_servers <- last_entry_per_ip_mult[last_entry_per_ip_mult$handshake == "SSLv3",]

insecure_records <- df[df$handshake == "SSLv3",]
df <- df[nchar(levels(df$timestamp)[as.numeric(df$timestamp)]) > 3,]
secure_records <- df[df$handshake != "SSLv3",]

count_downgrades_per_day <- ddply(df, .(date_round_day), summarize, count_sslv3_updated_server=sum(ip %in% ips_using_secure_handshake & handshake == "SSLv3"))
count_downgrades_per_day <- ddply(df, .(date_round_day), summarize, count_sslv3_updated_server=sum(handshake == "SSLv3" && ip %in% secure_ips_before(secure_records, posix_date)))


# Creating plots
ggplot(data=count_handshake_day, aes(x=date_round_day, y=count)) + 
    geom_line() + 
    geom_point() + 
    ylab(label="Number of connections") +
    xlab(label="Time") +
    ggtitle("Number of connections per day") +
    ggsave("count_day.png")

ggplot(data=count_handshake_week, aes(x=date_round_week, y=count)) + 
    geom_line() + 
    geom_point() + 
    ylab(label="Number of connections") +
    xlab(label="Time") +
    ggtitle("Number of connections per week") +
    ggsave("count_week.png")

ggplot(data=count_handshake_day, aes(x=date_round_day, y=count_ssl3)) + 
    geom_line() + 
    geom_point() + 
    ylab(label="Number of connections using SSLv3") +
    xlab(label="Time") +
    ggtitle("Number of connections using SSLv3 per day") +
    ggsave("sslv3_count_day.png")

ggplot(data=count_handshake_week, aes(x=date_round_week, y=count_ssl3)) + 
    geom_line() + 
    geom_point() + 
    ylab(label="Number of connections using SSLv3") +
    xlab(label="Time") +
    ggtitle("Number of connections using SSLv3 per week") +
    ggsave("sslv3_count_week.png")

ggplot(data=count_handshake_week, aes(x=date_round_week, y=relative_ssl3)) + 
    geom_line() + 
    geom_point() + 
    ylab(label="Relative number of connections using SSLv3") +
    xlab(label="Time") +
    ggtitle("Relative number of connections using SSLv3 per week") +
    ggsave("sslv3_relative_week.png")

ggplot(data=count_handshake_day, aes(x=date_round_day, y=relative_ssl3)) + 
    geom_line() + 
    geom_point() + 
    ylab(label="Relative number of connections using SSLv3") +
    xlab(label="Time") +
    ggtitle("Relative number of connections using SSLv3 per day") +
    ggsave("sslv3_relative_day.png")