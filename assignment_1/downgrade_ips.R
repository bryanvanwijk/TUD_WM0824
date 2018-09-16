Sys.setenv('R_MAX_VSIZE'=64000000000)
library(dplyr)
# Load data set
mydata <- read.csv("poodlessl.csv", header=TRUE)

# There are lines that have more attributes than required
# (eg. Lines that contain things like HTTP request code, cookies, etc, after the device_serial_attribute).
# The extra attributes get read as their own incorrect lines; Therefore, filter them out
df <- mydata[nchar(levels(mydata$timestamp)[as.numeric(mydata$timestamp)]) > 3,]

# Remove the original data set from memory; we don't need it anymore.
rm(mydata)

# Add a POSIX timestamp column to make date comparisons simpler
df$posix_timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%OS")

#ips <- data.frame(table(df$ip))
#handshakes <- data.frame(table(df$handshake))

# Count of number of handshakes per protocol
handshakes <- df %>% group_by(df$handshake) %>% summarise(count = n())

ips <- df %>% arrange(ip, port, posix_timestamp) %>% group_by(ip, port)

#ips_that_changed_back <- ips %>% filter(handshake == "SSLv3" & lag(handshake) != "SSLv3")

last_entry_per_server <- ips %>% filter(posix_timestamp == max(posix_timestamp))

currently_insecure_servers <- last_entry_per_server[last_entry_per_server$handshake == "SSLv3",]
currently_secure_servers <- last_entry_per_server[last_entry_per_server$handshake != "SSLv3",]
insecure_servers_by_country <- currently_insecure_servers %>% group_by(subject_country) %>% summarise(count = n())
#insecure_servers_no_country <- currently_insecure_servers[currently_insecure_servers$subject_country == "",]


#ddply(df, .(posix_timestamp), summarize, percentage = sum(handshake != "SSLv3"))

#View(ips)
#mydata[mydata$ip == "104.103.178.77",]
#ip_104_103_178_77 <- mydata[mydata$ip == "104.103.178.77",]
#View(ip_104_103_178_77)
#ip_13_95_150_238 <- mydata[mydata$ip == "13.95.150.238",]
#View(ip_13_95_150_238)

#hosts <- subset(df, handshake == 'SSLv3')
#ipAddresses <- unique(hosts$ip)
  
#last_entry_per_ip_mult <- df %>% group_by(ip) %>% filter(n() > 1) %>% filter(posix_timestamp == max(posix_timestamp))

ips_with_mult_entries <- ips %>% filter(n() > 1)
servers_that_used_secure_comm <- ips_with_mult_entries[ips_with_mult_entries$handshake != "SSLv3",]
servers_that_used_secure_comm <- servers_that_used_secure_comm %>% select(ip, port)
servers_that_used_secure_comm <- unique(servers_that_used_secure_comm)
ips_that_reverted_back_to_sslv3 <- inner_join(currently_insecure_servers, servers_that_used_secure_comm, by=c("ip", "port"))