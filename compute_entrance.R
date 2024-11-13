#--------------------------------
# Reading highway entrances and
# compute travel time from muni
#--------------------------------

#--------------------------------
# Packages
#--------------------------------
library(sf)
library(opentripplanner)

#--------------------------------
# Load data
#--------------------------------
# Highway entrances location
points_entrees = st_read("/Users/rloup/Desktop/autoroutes/troncons/centre_entrees_100_corr.shp")
dim(points_entrees)

# Municipality centers
communes_centres = read.csv("/Users/rloup/Documents/r_projects/spatial_autocorrelation_political_opinions/data/distances/communes_ch_centres2023.csv")
dim(communes_centres)

#--------------------------------
# Transform data
#--------------------------------
# WGS84 coordinate system
communes_sf = st_as_sf(communes_centres, coords = c("longitude", "latitude"), crs = st_crs(4326))
points_entrees_WGS = st_transform(points_entrees, crs = st_crs(4326))

# Plot municipalities (in black) and entrances (in red)
plot(communes_sf$geometry, pch = 19, cex = 0.5)
plot(points_entrees_WGS$geometry, cex = 0.5, add = T, col = "red")

#--------------------------------
# Computation
#--------------------------------
# Geodesic distance matrix from municipality to highway entrance
distance_commune_to_entree = st_distance(points_entrees_WGS, communes_sf)
View(distance_commune_to_entree)

min_commune_to_entree = apply(distance_commune_to_entree, 2, min)
View(as.matrix(min_commune_to_entree))

# Choose the number of closest geodesic distance
n_dist = 19
# Trouver les indices des "n_dist" distances minimales pour chaque colonne
# min_indices = apply(distance_commune_to_entree, 2, function(colonne) order(colonne)[1:n_dist])
min_indices = apply(distance_commune_to_entree, 2, function(colonne) order(colonne)[n_dist])
# Trouver les "n_dist" distances minimales pour chaque colonne
min_distances <- apply(distance_commune_to_entree, 2, function(colonne) sort(colonne)[6:n_dist])

# Municipalities close or far from an entrance
index_far = which(min_commune_to_entree > 10000)
index_close = which(min_commune_to_entree < 10000)

#--------------------------------
# OTP computation
#--------------------------------
# --- OTP routing
# Paths to OTP
path_data <- file.path("/Users/rloup/otp2024")
path_otp <- file.path("/Users/rloup/otp2024/otp-2.2.0-shaded.jar")
# /!\ Pay attention to the Java version -> Java 8 for OTP 1.x
# Can be open on R in the Terminal to choose Java version
# Can also be better with the 1.5 OTP rather than 2.x

# Run OTP
# Build graph (only one time)
# Create on Graph on terminal
# java -Xmx28G -Xverify:none -jar /Users/rloup/otp2024/otp-2.2.0-shaded.jar "$@" --build --save /Users/rloup/otp2024/graphs/default

trace("otp_check_java",edit=TRUE) # -> java 17 instead java 11, line 38
trace("otp_connect",edit=TRUE)  # -> otpcon$otp_version <- 2.2, line 24

log2 <- otp_setup(otp = path_otp, dir = path_data, memory = 30000, port = 8080, router = "default")
otpcon <- otp_connect()

# otpcon$otp_version = 2

# Connection test
test = otp_plan(otpcon, 
         fromPlace = c(6.624411, 46.516710), 
         toPlace = c(6.619409, 46.516636),
         mode = "CAR")

# Transform data into simple feature
sf_communesCH = st_as_sf(communes_centres,
                         coords = c("longitude", "latitude"), crs = 4326)

# Origin and destination
fromPlace = sf_communesCH
toPlace   = points_entrees_WGS[min_indices,]

# Multi-core routing
mc.cores = parallel::detectCores()
routes_n19 <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace,
                   toPlace = points_entrees_WGS[min_indices,],
                   fromID = as.character(fromPlace$BFS_NUMMER),
                   toID = points_entrees_WGS[min_indices,]$UUID,
                   get_geometry = FALSE,
                   distance_balance = FALSE,
                   ncores = mc.cores)
dim(routes_n19)

# See if all destinations are reachable
lostMuni = which(fromPlace$BFS_NUMMER %in% setdiff(fromPlace$BFS_NUMMER, routes_n20$fromPlace))

# Do the computation again if missed routes
# Compute routes
route_tot <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace[lostMuni,],
                   toPlace = toPlace[lostMuni,],
                   fromID = as.character(fromPlace[lostMuni,]$BFS_NUMMER),
                   toID = toPlace[lostMuni,]$UUID,
                   get_geometry = FALSE,
                   distance_balance = FALSE,
                   ncores = mc.cores)

route_tot3 = rbind(routes_n20, route_tot)
dim(route_tot3)
# write.csv(routes_n6, "route_tot6.csv", row.names = F)

route_tot_sorted = df_sorted <- route_tot3[order(as.numeric(route_tot3$fromPlace)), ]
dim(route_tot_sorted)
View(route_tot_sorted)

# route_tot2_sorted = df_sorted <- route_tot2[order(as.numeric(route_tot2$fromPlace)), ]
# route_tot3_sorted = df_sorted <- route_tot3[order(as.numeric(route_tot3$fromPlace)), ]
# routes_n4_sorted = df_sorted <- routes_n4[order(as.numeric(routes_n4$fromPlace)), ]
# routes_n5_sorted = df_sorted <- routes_n5[order(as.numeric(routes_n5$fromPlace)), ]
# routes_n6_sorted = df_sorted <- routes_n6[order(as.numeric(routes_n6$fromPlace)), ]

# Bind different iterations
min_time = as.data.frame(cbind(as.numeric(route_tot_sorted$fromPlace), as.numeric(route_tot_sorted$duration)))

min_time = cbind(as.numeric(route_tot_sorted$fromPlace), as.numeric(route_tot_sorted$duration), 
                 as.numeric(route_tot2_sorted$duration), as.numeric(route_tot3_sorted$duration), 
                 as.numeric(routes_n4_sorted$duration), as.numeric(routes_n5_sorted$duration), 
                 as.numeric(routes_n6_sorted$duration))
colnames(min_time) = c("BFS_n", "t1", "t2", "t3", "t4", "t5", "t6")
View(min_time)

names(min_time) = c("BFS", "t20")
min_time_to_highway$t20 = min_time$t20
names(min_time_to_highway)

View(min_time_to_highway)

write.csv(min_time_to_highway, "min_time_to_highway3.csv", row.names = F)

# --- Distances and times, save results
min_distance_to_highway = read.csv("/Users/rloup/Desktop/autoroutes/min_distance_to_highway.csv")

min_distance = as.data.frame(cbind(as.numeric(route_tot_sorted$fromPlace), as.numeric(route_tot_sorted$leg_distance)))

min_distance = cbind(as.numeric(route_tot_sorted$fromPlace), as.numeric(route_tot_sorted$leg_distance), 
                 as.numeric(route_tot2_sorted$leg_distance), as.numeric(route_tot3_sorted$leg_distance), 
                 as.numeric(routes_n4_sorted$leg_distance), as.numeric(routes_n5_sorted$leg_distance), 
                 as.numeric(routes_n6_sorted$leg_distance))
colnames(min_distance) = c("BFS_n", "d1", "d2", "d3", "d4", "d5", "d6")
write.csv(min_time, "min_distance_to_highway.csv", row.names = F)
View(min_distance)
