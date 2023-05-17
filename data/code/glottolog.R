# Load packages
source("00_load_packages.R")

devtools::install_github("erichround/glottoTrees",
                         dependencies = T, 
                         INSTALL_opts = c("--no-multiarch"))



library(glottoTrees)

tree_GA <- get_glottolog_trees("Indo-European")
tree_GA_abr <- abridge_labels(tree_GA)

teste <- keep_as_tip(tree_GA_abr, label = c("pugl1238", "nort2612", "cast1245", 
                                         "moli1245", "napo1241", "sici1248", 
                                         "anat1257", "uncl1527", "alba1267", 
                                         "arme1241", "balt1263", "celt1248", 
                                         "germ1287", "grae1234", "indo1320", 
                                         "tokh1241", "sabe1249", "uncl1536", 
                                         "vene1257", "fali1291", "oldl1238", 
                                         "lati1261", "sout3158", "east2714", 
                                         "west2813", "dalm1244", "ital1282"))

final <- relabel_with_names(teste)
plot_glotto(final, srt = 35, extra_height = -5)

aiai <- rescale_branches_exp(final)
aiai<- rescale_deepest_branches(aiai)
aiai <- ultrametricize(aiai)
plot_glotto(aiai, srt = 35)


tree_GA <- get_glottolog_trees("GreatAndamanese")
tree_GA_abr <- abridge_labels(tree_GA)
plot_glotto(tree_GA_abr)


teste <- keep_tip(tree_GA_abr, label = c("mixe1288", "akak1251"))

plot_glotto(teste)



