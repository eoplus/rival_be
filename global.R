
get_olci_prod <- function(x) {
  switch(x,
         "Chl a (OC4Me)" = "olci_chla_oc", 
         "Chl a (NN)" = "olci_chla_nn", 
         "SPM (NN)" = "olci_tsm", 
         "PAR" = "olci_par",
         "Rrs(400)" = "olci_Rrs400", 
         "Rrs(412)" = "olci_Rrs412", 
         "Rrs(442)" = "olci_Rrs442", 
         "Rrs(490)" = "olci_Rrs490",
         "Rrs(510)" = "olci_Rrs510", 
         "Rrs(560)" = "olci_Rrs560", 
         "Rrs(620)" = "olci_Rrs620", 
         "Rrs(665)" = "olci_Rrs665", 
         "Rrs(674)" = "olci_Rrs674", 
         "Rrs(681)" = "olci_Rrs681", 
         "Rrs(709)" = "olci_Rrs709", 
         "Rrs(754)" = "olci_Rrs754", 
         "Rrs(779)" = "olci_Rrs779", 
         "Rrs(865)" = "olci_Rrs865", 
         "Rrs(885)" = "olci_Rrs885", 
         "Rrs(1020)" = "olci_Rrs1020"
  )
}

get_olci_insitu_prod <- function(x) {
  switch(x,
         "Chl a (OC4Me)" = "Chlorophyll_a", 
         "Chl a (NN)" = "Chlorophyll_a", 
         "SPM (NN)" = "SPM", 
         "PAR" = "PAR",
         "Rrs(400)" = "Rrs400", 
         "Rrs(412)" = "Rrs412", 
         "Rrs(442)" = "Rrs443", 
         "Rrs(490)" = "Rrs490",
         "Rrs(510)" = "Rrs510", 
         "Rrs(560)" = "Rrs560", 
         "Rrs(620)" = "Rrs620", 
         "Rrs(665)" = "Rrs665", 
         "Rrs(674)" = "Rrs674", 
         "Rrs(681)" = "Rrs681", 
         "Rrs(709)" = "Rrs709", 
         "Rrs(754)" = "Rrs754", 
         "Rrs(779)" = "Rrs779", 
         "Rrs(865)" = "Rrs865", 
         "Rrs(885)" = "Rrs885", 
         "Rrs(1020)" = "Rrs1020"
  )
}

get_olci_xylab <- function(x) {
  switch(x,
         "Chl a (OC4Me)" = "mg/m3", 
         "Chl a (NN)" = "mg/m3", 
         "SPM (NN)" = "g/m3", 
         "PAR" = "muEinstein/m2",
         "Rrs(400)" = "1/sr", 
         "Rrs(412)" = "1/sr", 
         "Rrs(442)" = "1/sr", 
         "Rrs(490)" = "1/sr",
         "Rrs(510)" = "1/sr", 
         "Rrs(560)" = "1/sr", 
         "Rrs(620)" = "1/sr", 
         "Rrs(665)" = "1/sr", 
         "Rrs(674)" = "1/sr", 
         "Rrs(681)" = "1/sr", 
         "Rrs(709)" = "1/sr", 
         "Rrs(754)" = "1/sr", 
         "Rrs(779)" = "1/sr", 
         "Rrs(865)" = "1/sr", 
         "Rrs(885)" = "1/sr", 
         "Rrs(1020)" = "1/sr"
  )
}
