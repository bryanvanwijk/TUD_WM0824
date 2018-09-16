# This script is used to determine the number of servers that are using the insecure SSLv3 handshake protocol at the end
# of the data set provided, grouped by country. It is also used to determine the servers that was previously on a secure
# handshake protocol, but has since reverted to using SSLv3.

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

# We define a server as a (IP, port) pair, since one IP could be the host of multiple servers on different ports.
servers <- df %>% arrange(ip, port, posix_timestamp) %>% group_by(ip, port)

# Generate a list of the very last entry we have for each server in the dataset.
last_entry_per_server <- servers %>% filter(posix_timestamp == max(posix_timestamp))

currently_insecure_servers <- last_entry_per_server[last_entry_per_server$handshake == "SSLv3",]
currently_secure_servers <- last_entry_per_server[last_entry_per_server$handshake != "SSLv3",]
insecure_servers_by_country <- currently_insecure_servers %>% group_by(subject_country) %>% summarise(count = n())

# We only wish to look at servers that actually have multiple entries associated with them, since
# a server can't go from using something secure to SSLv3 if there's only ever one entry for it.
servers_with_mult_entries <- servers %>% filter(n() > 1)
servers_that_used_secure_comm <- servers_with_mult_entries[servers_with_mult_entries$handshake != "SSLv3",]
servers_that_used_secure_comm <- servers_that_used_secure_comm %>% select(ip, port)
servers_that_used_secure_comm <- unique(servers_that_used_secure_comm)

# Determine the servers that reverted back to using SSLv3 after using a more secure handshake protocol
servers_that_reverted_back_to_sslv3 <- inner_join(currently_insecure_servers, servers_that_used_secure_comm, by=c("ip", "port"))
