pncp <- read.csv("base_pncp_corrigidos.csv", sep = ",")

lai <- read.csv("base_nomes_corrigidos.csv", sep = ",")


base_completa <- rbind(pncp, lai)
write_csv(base_completa, "base_total.csv")
