#Llamar a R:
module load R-bundle-CRAN

#Llamar al script:
R CMD BATCH --no-save --no-restore /home/m.gonzalez/MOSQUITO_ACTIVITY_BCN/test_cluster.R job.out
