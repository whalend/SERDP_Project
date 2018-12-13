#### measuRing inital ring measurements ####

####+ import packages ####
library(measuRing)
library(plyr)
library(tidyr)
library(readr)
citation("measuRing")

#### TREE SAMPLE 1-S_DC ####

tree_1_S_DC_one <- ('tree_rings/pictures/1-S_DC_crossSection1.png')
ring_1_S_DC_one <- ringDetect(image = tree_1_S_DC_one, segs =1, ppi = 600, last.yr = 2018)
incl_1_S_DC_one <- c(247, 294, 458, 570, 678, 737, 940)
excl_1_S_DC_one <- as.numeric(attr(ring_1_S_DC_one, "coln"))[!(as.numeric(attr(ring_1_S_DC_one, "coln")) %in% incl_1_S_DC_one)]
updated_1_S_DC_one <- update(ring_1_S_DC_one, inclu = incl_1_S_DC_one)
updated_1_S_DC_one <- update(updated_1_S_DC_one, exclu = excl_1_S_DC_one)

widths_1_S_DC_one <- ringWidths(tree_1_S_DC_one, inclu = incl_1_S_DC_one, exclu = excl_1_S_DC_one, last.yr = 2018, ppi = 600)

# tree_1_S_DC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_1_S_DC_two <- ringDetect(image = tree_1_S_DC_two, ppi = 600, last.yr = 2018)
# incl_1_S_DC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_1_S_DC_two <- as.numeric(attr(ring_1_S_DC_two, "coln"))[!(as.numeric(attr(ring_1_S_DC_two, "coln")) %in% incl_1_S_DC_two)]
# updated_1_S_DC_two <- update(ring_1_S_DC_two, inclu = incl_1_S_DC_two)
# updated_1_S_DC_two <- update(updated_1_S_DC_two, exclu = excl_1_S_DC_two)
# 
# widths_1_S_DC_two <- ringWidths(tree_1_S_DC_two, inclu = incl_1_S_DC_two, exclu = excl_1_S_DC_two, last.yr = 2018, ppi = 600)
# 
# tree_1_S_DC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_1_S_DC_three <- ringDetect(image = tree_1_S_DC_three, ppi = 600, last.yr = 2018)
# incl_1_S_DC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_1_S_DC_three <- as.numeric(attr(ring_1_S_DC_three, "coln"))[!(as.numeric(attr(ring_1_S_DC_three, "coln")) %in% incl_1_S_DC_three)]
# updated_1_S_DC_three <- update(ring_1_S_DC_three, inclu = incl_1_S_DC_three)
# updated_1_S_DC_three <- update(updated_1_S_DC_three, exclu = excl_1_S_DC_three)
# 
# widths_1_S_DC_three <- ringWidths(tree_1_S_DC_three, inclu = incl_1_S_DC_three, exclu = excl_1_S_DC_three, last.yr = 2018, ppi = 600)
# 
# tree_1_S_DC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_1_S_DC_four <- ringDetect(image = tree_1_S_DC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_1_S_DC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_1_S_DC_four <- as.numeric(attr(ring_1_S_DC_four, "coln"))[!(as.numeric(attr(ring_1_S_DC_four, "coln")) %in% incl_1_S_DC_four)]
# updated_1_S_DC_four <- update(ring_1_S_DC_four, inclu = incl_1_S_DC_four)
# updated_1_S_DC_four <- update(updated_1_S_DC_four, exclu = excl_1_S_DC_four)
# 
# widths_1_S_DC_four <- ringWidths(tree_1_S_DC_four, inclu = incl_1_S_DC_four, exclu = excl_1_S_DC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_1_S_DC <- plyr::join_all(list(widths_1_S_DC_one, widths_1_S_DC_two, widths_1_S_DC_three, widths_1_S_DC_four))
# 
# widths_1_S_DC <- tidyr::gather(widths_1_S_DC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_1_S_DC, path = "tree_rings/data/indv_rings/widths_1_S_DC")
# 
# 
# write_csv(widths_1_S_DC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 2-B_AN ####

tree_2_B_AN_one <- ('tree_rings/pictures/2-B_AN_crossSection1.png')
ring_2_B_AN_one <- ringDetect(image = tree_2_B_AN_one, segs =1, ppi = 600, last.yr = 2018)
incl_2_B_AN_one <- c(107, 159, 350, 537, 720, 983)
excl_2_B_AN_one <- as.numeric(attr(ring_2_B_AN_one, "coln"))[!(as.numeric(attr(ring_2_B_AN_one, "coln")) %in% incl_2_B_AN_one)]
updated_2_B_AN_one <- update(ring_2_B_AN_one, inclu = incl_2_B_AN_one)
updated_2_B_AN_one <- update(updated_2_B_AN_one, exclu = excl_2_B_AN_one)

widths_2_B_AN_one <- ringWidths(tree_2_B_AN_one, inclu = incl_2_B_AN_one, exclu = excl_2_B_AN_one, last.yr = 2018, ppi = 600)

# tree_2_B_AN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_2_B_AN_two <- ringDetect(image = tree_2_B_AN_two, ppi = 600, last.yr = 2018)
# incl_2_B_AN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_2_B_AN_two <- as.numeric(attr(ring_2_B_AN_two, "coln"))[!(as.numeric(attr(ring_2_B_AN_two, "coln")) %in% incl_2_B_AN_two)]
# updated_2_B_AN_two <- update(ring_2_B_AN_two, inclu = incl_2_B_AN_two)
# updated_2_B_AN_two <- update(updated_2_B_AN_two, exclu = excl_2_B_AN_two)
# 
# widths_2_B_AN_two <- ringWidths(tree_2_B_AN_two, inclu = incl_2_B_AN_two, exclu = excl_2_B_AN_two, last.yr = 2018, ppi = 600)
# 
# tree_2_B_AN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_2_B_AN_three <- ringDetect(image = tree_2_B_AN_three, ppi = 600, last.yr = 2018)
# incl_2_B_AN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_2_B_AN_three <- as.numeric(attr(ring_2_B_AN_three, "coln"))[!(as.numeric(attr(ring_2_B_AN_three, "coln")) %in% incl_2_B_AN_three)]
# updated_2_B_AN_three <- update(ring_2_B_AN_three, inclu = incl_2_B_AN_three)
# updated_2_B_AN_three <- update(updated_2_B_AN_three, exclu = excl_2_B_AN_three)
# 
# widths_2_B_AN_three <- ringWidths(tree_2_B_AN_three, inclu = incl_2_B_AN_three, exclu = excl_2_B_AN_three, last.yr = 2018, ppi = 600)
# 
# tree_2_B_AN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_2_B_AN_four <- ringDetect(image = tree_2_B_AN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_2_B_AN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_2_B_AN_four <- as.numeric(attr(ring_2_B_AN_four, "coln"))[!(as.numeric(attr(ring_2_B_AN_four, "coln")) %in% incl_2_B_AN_four)]
# updated_2_B_AN_four <- update(ring_2_B_AN_four, inclu = incl_2_B_AN_four)
# updated_2_B_AN_four <- update(updated_2_B_AN_four, exclu = excl_2_B_AN_four)
# 
# widths_2_B_AN_four <- ringWidths(tree_2_B_AN_four, inclu = incl_2_B_AN_four, exclu = excl_2_B_AN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_2_B_AN <- plyr::join_all(list(widths_2_B_AN_one, widths_2_B_AN_two, widths_2_B_AN_three, widths_2_B_AN_four))
# 
# widths_2_B_AN <- tidyr::gather(widths_2_B_AN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_2_B_AN, path = "tree_rings/data/indv_rings/widths_2_B_AN")
# 
# write_csv(widths_2_B_AN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 2-T_AN ####

tree_2_T_AN_one <- ('tree_rings/pictures/2-T_AN_crossSection1.png')
ring_2_T_AN_one <- ringDetect(image = tree_2_T_AN_one, segs =1, ppi = 600, last.yr = 2018)
incl_2_T_AN_one <- c(38, 101, 307, 484, 628, 722, 812)
excl_2_T_AN_one <- as.numeric(attr(ring_2_T_AN_one, "coln"))[!(as.numeric(attr(ring_2_T_AN_one, "coln")) %in% incl_2_T_AN_one)]
updated_2_T_AN_one <- update(ring_2_T_AN_one, inclu = incl_2_T_AN_one)
updated_2_T_AN_one <- update(updated_2_T_AN_one, exclu = excl_2_T_AN_one)

widths_2_T_AN_one <- ringWidths(tree_2_T_AN_one, inclu = incl_2_T_AN_one, exclu = excl_2_T_AN_one, last.yr = 2018, ppi = 600)

tree_2_T_AN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
ring_2_T_AN_two <- ringDetect(image = tree_2_T_AN_two, ppi = 600, last.yr = 2018)
incl_2_T_AN_two <- c(15, 86, 272, 456, 590, 672, 775)
excl_2_T_AN_two <- as.numeric(attr(ring_2_T_AN_two, "coln"))[!(as.numeric(attr(ring_2_T_AN_two, "coln")) %in% incl_2_T_AN_two)]
updated_2_T_AN_two <- update(ring_2_T_AN_two, inclu = incl_2_T_AN_two)
updated_2_T_AN_two <- update(updated_2_T_AN_two, exclu = excl_2_T_AN_two)

widths_2_T_AN_two <- ringWidths(tree_2_T_AN_two, inclu = incl_2_T_AN_two, exclu = excl_2_T_AN_two, last.yr = 2018, ppi = 600)

tree_2_T_AN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
ring_2_T_AN_three <- ringDetect(image = tree_2_T_AN_three, ppi = 600, last.yr = 2018)
incl_2_T_AN_three <- c(17, 70, 219, 342, 449, 534, 632)
excl_2_T_AN_three <- as.numeric(attr(ring_2_T_AN_three, "coln"))[!(as.numeric(attr(ring_2_T_AN_three, "coln")) %in% incl_2_T_AN_three)]
updated_2_T_AN_three <- update(ring_2_T_AN_three, inclu = incl_2_T_AN_three)
updated_2_T_AN_three <- update(updated_2_T_AN_three, exclu = excl_2_T_AN_three)

widths_2_T_AN_three <- ringWidths(tree_2_T_AN_three, inclu = incl_2_T_AN_three, exclu = excl_2_T_AN_three, last.yr = 2018, ppi = 600)

tree_2_T_AN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
ring_2_T_AN_four <- ringDetect(image = tree_2_T_AN_four, segs =1, ppi = 600, last.yr = 2018)
incl_2_T_AN_four <- c(209, 302, 441, 601, 725, 811, 919)
excl_2_T_AN_four <- as.numeric(attr(ring_2_T_AN_four, "coln"))[!(as.numeric(attr(ring_2_T_AN_four, "coln")) %in% incl_2_T_AN_four)]
updated_2_T_AN_four <- update(ring_2_T_AN_four, inclu = incl_2_T_AN_four)
updated_2_T_AN_four <- update(updated_2_T_AN_four, exclu = excl_2_T_AN_four)

widths_2_T_AN_four <- ringWidths(tree_2_T_AN_four, inclu = incl_2_T_AN_four, exclu = excl_2_T_AN_four, last.yr = 2018, ppi = 600)


widths_2_T_AN <- plyr::join_all(list(widths_2_T_AN_one, widths_2_T_AN_two, widths_2_T_AN_three, widths_2_T_AN_four))

widths_2_T_AN <- tidyr::gather(widths_2_T_AN, key = cs_id, value = ring_width_mm, -year)

write_csv(widths_2_T_AN, path = "tree_rings/data/indv_rings/widths_2_T_AN")

#### create master file, to append all others here ####

write_csv(widths_2_T_AN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 6-G_DN ####

tree_6_G_DN_one <- ('tree_rings/pictures/6-G_DN_crossSection1.png')
ring_6_G_DN_one <- ringDetect(image = tree_6_G_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_6_G_DN_one <- c(176, 310, 444, 507, 592, 714)
excl_6_G_DN_one <- as.numeric(attr(ring_6_G_DN_one, "coln"))[!(as.numeric(attr(ring_6_G_DN_one, "coln")) %in% incl_6_G_DN_one)]
updated_6_G_DN_one <- update(ring_6_G_DN_one, inclu = incl_6_G_DN_one)
updated_6_G_DN_one <- update(updated_6_G_DN_one, exclu = excl_6_G_DN_one)

widths_6_G_DN_one <- ringWidths(tree_6_G_DN_one, inclu = incl_6_G_DN_one, exclu = excl_6_G_DN_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 12-E_AC TURTLE BLANK ####

#### TREE SAMPLE 14-E_DC ####

tree_14_E_DC_one <- ('tree_rings/pictures/14-E_DC_crossSection1.png')
ring_14_E_DC_one <- ringDetect(image = tree_14_E_DC_one, segs =1, ppi = 600, last.yr = 2018)
incl_14_E_DC_one <- c(150, 166, 397, 570, 641, 837)
excl_14_E_DC_one <- as.numeric(attr(ring_14_E_DC_one, "coln"))[!(as.numeric(attr(ring_14_E_DC_one, "coln")) %in% incl_14_E_DC_one)]
updated_14_E_DC_one <- update(ring_14_E_DC_one, inclu = incl_14_E_DC_one)
updated_14_E_DC_one <- update(updated_14_E_DC_one, exclu = excl_14_E_DC_one)

widths_14_E_DC_one <- ringWidths(tree_14_E_DC_one, inclu = incl_14_E_DC_one, exclu = excl_14_E_DC_one, last.yr = 2018, ppi = 600)

# tree_14_E_DC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_14_E_DC_two <- ringDetect(image = tree_14_E_DC_two, ppi = 600, last.yr = 2018)
# incl_14_E_DC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_14_E_DC_two <- as.numeric(attr(ring_14_E_DC_two, "coln"))[!(as.numeric(attr(ring_14_E_DC_two, "coln")) %in% incl_14_E_DC_two)]
# updated_14_E_DC_two <- update(ring_14_E_DC_two, inclu = incl_14_E_DC_two)
# updated_14_E_DC_two <- update(updated_14_E_DC_two, exclu = excl_14_E_DC_two)
# 
# widths_14_E_DC_two <- ringWidths(tree_14_E_DC_two, inclu = incl_14_E_DC_two, exclu = excl_14_E_DC_two, last.yr = 2018, ppi = 600)
# 
# tree_14_E_DC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_14_E_DC_three <- ringDetect(image = tree_14_E_DC_three, ppi = 600, last.yr = 2018)
# incl_14_E_DC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_14_E_DC_three <- as.numeric(attr(ring_14_E_DC_three, "coln"))[!(as.numeric(attr(ring_14_E_DC_three, "coln")) %in% incl_14_E_DC_three)]
# updated_14_E_DC_three <- update(ring_14_E_DC_three, inclu = incl_14_E_DC_three)
# updated_14_E_DC_three <- update(updated_14_E_DC_three, exclu = excl_14_E_DC_three)
# 
# widths_14_E_DC_three <- ringWidths(tree_14_E_DC_three, inclu = incl_14_E_DC_three, exclu = excl_14_E_DC_three, last.yr = 2018, ppi = 600)
# 
# tree_14_E_DC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_14_E_DC_four <- ringDetect(image = tree_14_E_DC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_14_E_DC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_14_E_DC_four <- as.numeric(attr(ring_14_E_DC_four, "coln"))[!(as.numeric(attr(ring_14_E_DC_four, "coln")) %in% incl_14_E_DC_four)]
# updated_14_E_DC_four <- update(ring_14_E_DC_four, inclu = incl_14_E_DC_four)
# updated_14_E_DC_four <- update(updated_14_E_DC_four, exclu = excl_14_E_DC_four)
# 
# widths_14_E_DC_four <- ringWidths(tree_14_E_DC_four, inclu = incl_14_E_DC_four, exclu = excl_14_E_DC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_14_E_DC <- plyr::join_all(list(widths_14_E_DC_one, widths_14_E_DC_two, widths_14_E_DC_three, widths_14_E_DC_four))
# 
# widths_14_E_DC <- tidyr::gather(widths_14_E_DC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_14_E_DC, path = "tree_rings/data/indv_rings/widths_14_E_DC")
# 
# 
# write_csv(widths_14_E_DC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 14-T_DC ALL NOT WORKING ####

# tree_14_T_DC_one <- ('tree_rings/pictures/14-T_DC_crossSection1.png')
# ring_14_T_DC_one <- ringDetect(image = tree_14_T_DC_one, segs =1, ppi = 600, last.yr = 2018)
# incl_14_T_DC_one <- c(99, 170, 364, 533, 630, 810)
# excl_14_T_DC_one <- as.numeric(attr(ring_14_T_DC_one, "coln"))[!(as.numeric(attr(ring_14_T_DC_one, "coln")) %in% incl_14_T_DC_one)]
# updated_14_T_DC_one <- update(ring_14_T_DC_one, inclu = incl_14_T_DC_one)
# updated_14_T_DC_one <- update(updated_14_T_DC_one, exclu = excl_14_T_DC_one)
# 
# widths_14_T_DC_one <- ringWidths(tree_14_T_DC_one, inclu = incl_14_T_DC_one, exclu = excl_14_T_DC_one, last.yr = 2018, ppi = 600)
# 
# tree_14_T_DC_two <- ('tree_rings/pictures/14-T_DC_crossSection2.png')
# ring_14_T_DC_two <- ringDetect(image = tree_14_T_DC_two, ppi = 600, last.yr = 2018)
# 
# incl_14_T_DC_two <- c(92, 162, 314, 410, 487, 628)
# excl_14_T_DC_two <- as.numeric(attr(ring_14_T_DC_two, "coln"))[!(as.numeric(attr(ring_14_T_DC_two, "coln")) %in% incl_14_T_DC_two)]
# updated_14_T_DC_two <- update(ring_14_T_DC_two, inclu = incl_14_T_DC_two)
# updated_14_T_DC_two <- update(updated_14_T_DC_two, exclu = excl_14_T_DC_two)
# 
# widths_14_T_DC_two <- ringWidths(tree_14_T_DC_two, inclu = incl_14_T_DC_two, exclu = excl_14_T_DC_two, last.yr = 2018, ppi = 600)
# 
# tree_14_T_DC_three <- ('tree_rings/14-T_DC_crossSection3.png')
# ring_14_T_DC_three <- ringDetect(image = tree_14_T_DC_three, ppi = 600, last.yr = 2018)
# incl_14_T_DC_three <- c(121, 174, 375, 499, 570, 723)
# excl_14_T_DC_three <- as.numeric(attr(ring_16_E_DN_three, "coln"))[!(as.numeric(attr(ring_14_T_DC_three, "coln")) %in% incl_14_T_DC_three)]
# updated_14_T_DC_three <- update(ring_14_T_DC_three, inclu = incl_16_E_DN_three)
# updated_14_T_DC_three <- update(updated_14_T_DC_three, exclu = excl_16_E_DN_three)
# 
# widths_14_T_DC_three <- ringWidths(tree_14_T_DC_three, inclu = incl_14_T_DC_three, exclu = excl_14_T_DC_three, last.yr = 2018, ppi = 600)
# 
# tree_14_T_DC_four <- ('tree_rings/pictures/14-T_DC_crossSection4.png')
# ring_14_T_DC_four <- ringDetect(image = tree_14_T_DC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_14_T_DC_four <- c(115, 166, 305, 458, 545, 703)
# excl_14_T_DC_four <- as.numeric(attr(ring_14_T_DC_four, "coln"))[!(as.numeric(attr(ring_14_T_DC_four, "coln")) %in% incl_14_T_DC_four)]
# updated_14_T_DC_four <- update(ring_14_T_DC_four, inclu = incl_14_T_DC_four)
# updated_14_T_DC_four <- update(updated_14_T_DC_four, exclu = excl_14_T_DC_four)
# 
# widths_14_T_DC_four <- ringWidths(tree_14_T_DC_four, inclu = incl_14_T_DC_four, exclu = excl_14_T_DC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_14_T_DC <- plyr::join_all(list(widths_16_E_DN_one, widths_16_E_DN_two, widths_16_E_DN_three, widths_16_E_DN_four))
# 
# widths_14_T_DC <- tidyr::gather(widths_16_E_DN, key = cs_id, value = ring_width_mm, -year)

# # export 14_T_DC indv file
# 
# write_csv(widths_14_T_DC, path = "tree_rings/data/indv_rings/widths_14_T_DC")
# 
# # adding 14_T_DC to master
# 
# master <- write_csv(widths_14_T_DC, path = "tree_rings/data/tree_ring_widths_master", append = T)

#### TREE SAMPLE 16-E_DN ####

tree_16_E_DN_one <- ('tree_rings/pictures/16-E_DN_crossSection1.png')
ring_16_E_DN_one <- ringDetect(image = tree_16_E_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_16_E_DN_one <- c(99, 170, 364, 533, 630, 810)
excl_16_E_DN_one <- as.numeric(attr(ring_16_E_DN_one, "coln"))[!(as.numeric(attr(ring_16_E_DN_one, "coln")) %in% incl_16_E_DN_one)]
updated_16_E_DN_one <- update(ring_16_E_DN_one, inclu = incl_16_E_DN_one)
updated_16_E_DN_one <- update(updated_16_E_DN_one, exclu = excl_16_E_DN_one)

widths_16_E_DN_one <- ringWidths(tree_16_E_DN_one, inclu = incl_16_E_DN_one, exclu = excl_16_E_DN_one, last.yr = 2018, ppi = 600)

tree_16_E_DN_two <- ('tree_rings/pictures/16-E_DN_crossSection2.png')
ring_16_E_DN_two <- ringDetect(image = tree_16_E_DN_two, ppi = 600, last.yr = 2018)
incl_16_E_DN_two <- c(92, 162, 314, 410, 487, 628)
excl_16_E_DN_two <- as.numeric(attr(ring_16_E_DN_two, "coln"))[!(as.numeric(attr(ring_16_E_DN_two, "coln")) %in% incl_16_E_DN_two)]
updated_16_E_DN_two <- update(ring_16_E_DN_two, inclu = incl_16_E_DN_two)
updated_16_E_DN_two <- update(updated_16_E_DN_two, exclu = excl_16_E_DN_two)

widths_16_E_DN_two <- ringWidths(tree_16_E_DN_two, inclu = incl_16_E_DN_two, exclu = excl_16_E_DN_two, last.yr = 2018, ppi = 600)

tree_16_E_DN_three <- ('tree_rings/pictures/16-E_DN_crossSection3.png')
ring_16_E_DN_three <- ringDetect(image = tree_16_E_DN_three, ppi = 600, last.yr = 2018)
incl_16_E_DN_three <- c(121, 174, 375, 499, 570, 723)
excl_16_E_DN_three <- as.numeric(attr(ring_16_E_DN_three, "coln"))[!(as.numeric(attr(ring_16_E_DN_three, "coln")) %in% incl_16_E_DN_three)]
updated_16_E_DN_three <- update(ring_16_E_DN_three, inclu = incl_16_E_DN_three)
updated_16_E_DN_three <- update(updated_16_E_DN_three, exclu = excl_16_E_DN_three)

widths_16_E_DN_three <- ringWidths(tree_16_E_DN_three, inclu = incl_16_E_DN_three, exclu = excl_16_E_DN_three, last.yr = 2018, ppi = 600)

tree_16_E_DN_four <- ('tree_rings/pictures/16-E_DN_crossSection4.png')
ring_16_E_DN_four <- ringDetect(image = tree_16_E_DN_four, segs =1, ppi = 600, last.yr = 2018)
incl_16_E_DN_four <- c(115, 166, 305, 458, 545, 703)
excl_16_E_DN_four <- as.numeric(attr(ring_16_E_DN_four, "coln"))[!(as.numeric(attr(ring_16_E_DN_four, "coln")) %in% incl_16_E_DN_four)]
updated_16_E_DN_four <- update(ring_16_E_DN_four, inclu = incl_16_E_DN_four)
updated_16_E_DN_four <- update(updated_16_E_DN_four, exclu = excl_16_E_DN_four)

widths_16_E_DN_four <- ringWidths(tree_16_E_DN_four, inclu = incl_16_E_DN_four, exclu = excl_16_E_DN_four, last.yr = 2018, ppi = 600)


widths_16_E_DN <- plyr::join_all(list(widths_16_E_DN_one, widths_16_E_DN_two, widths_16_E_DN_three, widths_16_E_DN_four))

widths_16_E_DN <- tidyr::gather(widths_16_E_DN, key = cs_id, value = ring_width_mm, -year)

# export 16_E_DN indv file

write_csv(widths_16_E_DN, path = "tree_rings/data/indv_rings/widths_16_E_DN")

# adding 16_E_DN to master

master <- write_csv(widths_16_E_DN, path = "tree_rings/data/tree_ring_widths_master", append = T)

#### TREE SAMPLE 17-A_DN ####

tree_17_A_DN_one <- ('tree_rings/pictures/17-A_DN_crossSection1.png')
ring_17_A_DN_one <- ringDetect(image = tree_17_A_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_17_A_DN_one <- c(129, 156, 340, 483, 648, 895)
excl_17_A_DN_one <- as.numeric(attr(ring_17_A_DN_one, "coln"))[!(as.numeric(attr(ring_17_A_DN_one, "coln")) %in% incl_17_A_DN_one)]
updated_17_A_DN_one <- update(ring_17_A_DN_one, inclu = incl_17_A_DN_one)
updated_17_A_DN_one <- update(updated_17_A_DN_one, exclu = excl_17_A_DN_one)

widths_17_A_DN_one <- ringWidths(tree_17_A_DN_one, inclu = incl_17_A_DN_one, exclu = excl_17_A_DN_one, last.yr = 2018, ppi = 600)

# tree_17_A_DN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_17_A_DN_two <- ringDetect(image = tree_17_A_DN_two, ppi = 600, last.yr = 2018)
# incl_17_A_DN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_17_A_DN_two <- as.numeric(attr(ring_17_A_DN_two, "coln"))[!(as.numeric(attr(ring_17_A_DN_two, "coln")) %in% incl_17_A_DN_two)]
# updated_17_A_DN_two <- update(ring_17_A_DN_two, inclu = incl_17_A_DN_two)
# updated_17_A_DN_two <- update(updated_17_A_DN_two, exclu = excl_17_A_DN_two)
# 
# widths_17_A_DN_two <- ringWidths(tree_17_A_DN_two, inclu = incl_17_A_DN_two, exclu = excl_17_A_DN_two, last.yr = 2018, ppi = 600)
# 
# tree_17_A_DN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_17_A_DN_three <- ringDetect(image = tree_17_A_DN_three, ppi = 600, last.yr = 2018)
# incl_17_A_DN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_17_A_DN_three <- as.numeric(attr(ring_17_A_DN_three, "coln"))[!(as.numeric(attr(ring_17_A_DN_three, "coln")) %in% incl_17_A_DN_three)]
# updated_17_A_DN_three <- update(ring_17_A_DN_three, inclu = incl_17_A_DN_three)
# updated_17_A_DN_three <- update(updated_17_A_DN_three, exclu = excl_17_A_DN_three)
# 
# widths_17_A_DN_three <- ringWidths(tree_17_A_DN_three, inclu = incl_17_A_DN_three, exclu = excl_17_A_DN_three, last.yr = 2018, ppi = 600)
# 
# tree_17_A_DN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_17_A_DN_four <- ringDetect(image = tree_17_A_DN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_17_A_DN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_17_A_DN_four <- as.numeric(attr(ring_17_A_DN_four, "coln"))[!(as.numeric(attr(ring_17_A_DN_four, "coln")) %in% incl_17_A_DN_four)]
# updated_17_A_DN_four <- update(ring_17_A_DN_four, inclu = incl_17_A_DN_four)
# updated_17_A_DN_four <- update(updated_17_A_DN_four, exclu = excl_17_A_DN_four)
# 
# widths_17_A_DN_four <- ringWidths(tree_17_A_DN_four, inclu = incl_17_A_DN_four, exclu = excl_17_A_DN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_17_A_DN <- plyr::join_all(list(widths_17_A_DN_one, widths_17_A_DN_two, widths_17_A_DN_three, widths_17_A_DN_four))
# 
# widths_17_A_DN <- tidyr::gather(widths_17_A_DN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_17_A_DN, path = "tree_rings/data/indv_rings/widths_17_A_DN")
# 
# 
# write_csv(widths_17_A_DN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 17-F_DN ####

tree_17_F_DN_one <- ('tree_rings/pictures/17-F_DN_crossSection1.png')
ring_17_F_DN_one <- ringDetect(image = tree_17_F_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_17_F_DN_one <- c(105, 129, 208, 337, 480, 690)
excl_17_F_DN_one <- as.numeric(attr(ring_17_F_DN_one, "coln"))[!(as.numeric(attr(ring_17_F_DN_one, "coln")) %in% incl_17_F_DN_one)]
updated_17_F_DN_one <- update(ring_17_F_DN_one, inclu = incl_17_F_DN_one)
updated_17_F_DN_one <- update(updated_17_F_DN_one, exclu = excl_17_F_DN_one)

widths_17_F_DN_one <- ringWidths(tree_17_F_DN_one, inclu = incl_17_F_DN_one, exclu = excl_17_F_DN_one, last.yr = 2018, ppi = 600)

# tree_17_F_DN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_17_F_DN_two <- ringDetect(image = tree_17_F_DN_two, ppi = 600, last.yr = 2018)
# incl_17_F_DN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_17_F_DN_two <- as.numeric(attr(ring_17_F_DN_two, "coln"))[!(as.numeric(attr(ring_17_F_DN_two, "coln")) %in% incl_17_F_DN_two)]
# updated_17_F_DN_two <- update(ring_17_F_DN_two, inclu = incl_17_F_DN_two)
# updated_17_F_DN_two <- update(updated_17_F_DN_two, exclu = excl_17_F_DN_two)
# 
# widths_17_F_DN_two <- ringWidths(tree_17_F_DN_two, inclu = incl_17_F_DN_two, exclu = excl_17_F_DN_two, last.yr = 2018, ppi = 600)
# 
# tree_17_F_DN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_17_F_DN_three <- ringDetect(image = tree_17_F_DN_three, ppi = 600, last.yr = 2018)
# incl_17_F_DN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_17_F_DN_three <- as.numeric(attr(ring_17_F_DN_three, "coln"))[!(as.numeric(attr(ring_17_F_DN_three, "coln")) %in% incl_17_F_DN_three)]
# updated_17_F_DN_three <- update(ring_17_F_DN_three, inclu = incl_17_F_DN_three)
# updated_17_F_DN_three <- update(updated_17_F_DN_three, exclu = excl_17_F_DN_three)
# 
# widths_17_F_DN_three <- ringWidths(tree_17_F_DN_three, inclu = incl_17_F_DN_three, exclu = excl_17_F_DN_three, last.yr = 2018, ppi = 600)
# 
# tree_17_F_DN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_17_F_DN_four <- ringDetect(image = tree_17_F_DN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_17_F_DN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_17_F_DN_four <- as.numeric(attr(ring_17_F_DN_four, "coln"))[!(as.numeric(attr(ring_17_F_DN_four, "coln")) %in% incl_17_F_DN_four)]
# updated_17_F_DN_four <- update(ring_17_F_DN_four, inclu = incl_17_F_DN_four)
# updated_17_F_DN_four <- update(updated_17_F_DN_four, exclu = excl_17_F_DN_four)
# 
# widths_17_F_DN_four <- ringWidths(tree_17_F_DN_four, inclu = incl_17_F_DN_four, exclu = excl_17_F_DN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_17_F_DN <- plyr::join_all(list(widths_17_F_DN_one, widths_17_F_DN_two, widths_17_F_DN_three, widths_17_F_DN_four))
# 
# widths_17_F_DN <- tidyr::gather(widths_17_F_DN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_17_F_DN, path = "tree_rings/data/indv_rings/widths_17_F_DN")
# 
# 
# write_csv(widths_17_F_DN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 17-J_DN NOT WORKING ####

# tree_17_J_DN_one <- ('tree_rings/pictures/17-J_DN_crossSection1.png')
# ring_17_J_DN_one <- ringDetect(image = tree_17_J_DN_one, segs =1, ppi = 600, last.yr = 2018)
# incl_17_J_DN_one <- c(150, 166, 397, 570, 641, 837)
# excl_17_J_DN_one <- as.numeric(attr(ring_17_J_DN_one, "coln"))[!(as.numeric(attr(ring_17_J_DN_one, "coln")) %in% incl_17_J_DN_one)]
# updated_17_J_DN_one <- update(ring_17_J_DN_one, inclu = incl_17_J_DN_one)
# updated_17_J_DN_one <- update(updated_17_J_DN_one, exclu = excl_17_J_DN_one)
# 
# widths_17_J_DN_one <- ringWidths(tree_17_J_DN_one, inclu = incl_17_J_DN_one, exclu = excl_17_J_DN_one, last.yr = 2018, ppi = 600)

# tree_17_J_DN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_17_J_DN_two <- ringDetect(image = tree_17_J_DN_two, ppi = 600, last.yr = 2018)
# incl_17_J_DN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_17_J_DN_two <- as.numeric(attr(ring_17_J_DN_two, "coln"))[!(as.numeric(attr(ring_17_J_DN_two, "coln")) %in% incl_17_J_DN_two)]
# updated_17_J_DN_two <- update(ring_17_J_DN_two, inclu = incl_17_J_DN_two)
# updated_17_J_DN_two <- update(updated_17_J_DN_two, exclu = excl_17_J_DN_two)
# 
# widths_17_J_DN_two <- ringWidths(tree_17_J_DN_two, inclu = incl_17_J_DN_two, exclu = excl_17_J_DN_two, last.yr = 2018, ppi = 600)
# 
# tree_17_J_DN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_17_J_DN_three <- ringDetect(image = tree_17_J_DN_three, ppi = 600, last.yr = 2018)
# incl_17_J_DN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_17_J_DN_three <- as.numeric(attr(ring_17_J_DN_three, "coln"))[!(as.numeric(attr(ring_17_J_DN_three, "coln")) %in% incl_17_J_DN_three)]
# updated_17_J_DN_three <- update(ring_17_J_DN_three, inclu = incl_17_J_DN_three)
# updated_17_J_DN_three <- update(updated_17_J_DN_three, exclu = excl_17_J_DN_three)
# 
# widths_17_J_DN_three <- ringWidths(tree_17_J_DN_three, inclu = incl_17_J_DN_three, exclu = excl_17_J_DN_three, last.yr = 2018, ppi = 600)
# 
# tree_17_J_DN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_17_J_DN_four <- ringDetect(image = tree_17_J_DN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_17_J_DN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_17_J_DN_four <- as.numeric(attr(ring_17_J_DN_four, "coln"))[!(as.numeric(attr(ring_17_J_DN_four, "coln")) %in% incl_17_J_DN_four)]
# updated_17_J_DN_four <- update(ring_17_J_DN_four, inclu = incl_17_J_DN_four)
# updated_17_J_DN_four <- update(updated_17_J_DN_four, exclu = excl_17_J_DN_four)
# 
# widths_17_J_DN_four <- ringWidths(tree_17_J_DN_four, inclu = incl_17_J_DN_four, exclu = excl_17_J_DN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_17_J_DN <- plyr::join_all(list(widths_17_J_DN_one, widths_17_J_DN_two, widths_17_J_DN_three, widths_17_J_DN_four))
# 
# widths_17_J_DN <- tidyr::gather(widths_17_J_DN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_17_J_DN, path = "tree_rings/data/indv_rings/widths_17_J_DN")
# 
# 
# write_csv(widths_17_J_DN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 17-S_DN ####

tree_17_S_DN_one <- ('tree_rings/pictures/17-S_DN_crossSection1.png')
ring_17_S_DN_one <- ringDetect(image = tree_17_S_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_17_S_DN_one <- c(115, 148, 250, 330, 457, 490, 566)
excl_17_S_DN_one <- as.numeric(attr(ring_17_S_DN_one, "coln"))[!(as.numeric(attr(ring_17_S_DN_one, "coln")) %in% incl_17_S_DN_one)]
updated_17_S_DN_one <- update(ring_17_S_DN_one, inclu = incl_17_S_DN_one)
updated_17_S_DN_one <- update(updated_17_S_DN_one, exclu = excl_17_S_DN_one)

widths_17_S_DN_one <- ringWidths(tree_17_S_DN_one, inclu = incl_17_S_DN_one, exclu = excl_17_S_DN_one, last.yr = 2018, ppi = 600)

# tree_17_S_DN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_17_S_DN_two <- ringDetect(image = tree_17_S_DN_two, ppi = 600, last.yr = 2018)
# incl_17_S_DN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_17_S_DN_two <- as.numeric(attr(ring_17_S_DN_two, "coln"))[!(as.numeric(attr(ring_17_S_DN_two, "coln")) %in% incl_17_S_DN_two)]
# updated_17_S_DN_two <- update(ring_17_S_DN_two, inclu = incl_17_S_DN_two)
# updated_17_S_DN_two <- update(updated_17_S_DN_two, exclu = excl_17_S_DN_two)
# 
# widths_17_S_DN_two <- ringWidths(tree_17_S_DN_two, inclu = incl_17_S_DN_two, exclu = excl_17_S_DN_two, last.yr = 2018, ppi = 600)
# 
# tree_17_S_DN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_17_S_DN_three <- ringDetect(image = tree_17_S_DN_three, ppi = 600, last.yr = 2018)
# incl_17_S_DN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_17_S_DN_three <- as.numeric(attr(ring_17_S_DN_three, "coln"))[!(as.numeric(attr(ring_17_S_DN_three, "coln")) %in% incl_17_S_DN_three)]
# updated_17_S_DN_three <- update(ring_17_S_DN_three, inclu = incl_17_S_DN_three)
# updated_17_S_DN_three <- update(updated_17_S_DN_three, exclu = excl_17_S_DN_three)
# 
# widths_17_S_DN_three <- ringWidths(tree_17_S_DN_three, inclu = incl_17_S_DN_three, exclu = excl_17_S_DN_three, last.yr = 2018, ppi = 600)
# 
# tree_17_S_DN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_17_S_DN_four <- ringDetect(image = tree_17_S_DN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_17_S_DN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_17_S_DN_four <- as.numeric(attr(ring_17_S_DN_four, "coln"))[!(as.numeric(attr(ring_17_S_DN_four, "coln")) %in% incl_17_S_DN_four)]
# updated_17_S_DN_four <- update(ring_17_S_DN_four, inclu = incl_17_S_DN_four)
# updated_17_S_DN_four <- update(updated_17_S_DN_four, exclu = excl_17_S_DN_four)
# 
# widths_17_S_DN_four <- ringWidths(tree_17_S_DN_four, inclu = incl_17_S_DN_four, exclu = excl_17_S_DN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_17_S_DN <- plyr::join_all(list(widths_17_S_DN_one, widths_17_S_DN_two, widths_17_S_DN_three, widths_17_S_DN_four))
# 
# widths_17_S_DN <- tidyr::gather(widths_17_S_DN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_17_S_DN, path = "tree_rings/data/indv_rings/widths_17_S_DN")
# 
# 
# write_csv(widths_17_S_DN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 18-F_AN NOT WORKING ####

# tree_18_F_AN_one <- ('tree_rings/pictures/18-F_AN_crossSection1.png')
# ring_18_F_AN_one <- ringDetect(image = tree_18_F_AN_one, segs = 1, ppi = 600, last.yr = 2018)
# incl_18_F_AN_one <- c(176, 310, 444, 507, 592, 714)
# excl_18_F_AN_one <- as.numeric(attr(ring_18_F_AN_one, "coln"))[!(as.numeric(attr(ring_18_F_AN_one, "coln")) %in% incl_18_F_AN_one)]
# updated_18_F_AN_one <- update(ring_18_F_AN_one, inclu = incl_18_F_AN_one)
# updated_18_F_AN_one <- update(updated_18_F_AN_one, exclu = excl_18_F_AN_one)
# 
# widths_18_F_AN_one <- ringWidths(tree_18_F_AN_one, inclu = incl_18_F_AN_one, exclu = excl_18_F_AN_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 19-K_DC ####

tree_19_K_DC_one <- ('tree_rings/pictures/19-K_DC_crossSection1.png')
ring_19_K_DC_one <- ringDetect(image = tree_19_K_DC_one, segs =1, ppi = 600, last.yr = 2018)
incl_19_K_DC_one <- c(52, 195, 370, 440, 540, 650)
excl_19_K_DC_one <- as.numeric(attr(ring_19_K_DC_one, "coln"))[!(as.numeric(attr(ring_19_K_DC_one, "coln")) %in% incl_19_K_DC_one)]
updated_19_K_DC_one <- update(ring_19_K_DC_one, inclu = incl_19_K_DC_one)
updated_19_K_DC_one <- update(updated_19_K_DC_one, exclu = excl_19_K_DC_one)

widths_19_K_DC_one <- ringWidths(tree_19_K_DC_one, inclu = incl_19_K_DC_one, exclu = excl_19_K_DC_one, last.yr = 2018, ppi = 600)

tree_19_K_DC_two <- ('tree_rings/pictures/19-K_DC_crossSection2.png')
ring_19_K_DC_two <- ringDetect(image = tree_19_K_DC_two, ppi = 600, last.yr = 2018)
incl_19_K_DC_two <- c(55, 176, 321, 390, 470, 590)
excl_19_K_DC_two <- as.numeric(attr(ring_19_K_DC_two, "coln"))[!(as.numeric(attr(ring_19_K_DC_two, "coln")) %in% incl_19_K_DC_two)]
updated_19_K_DC_two <- update(ring_19_K_DC_two, inclu = incl_19_K_DC_two)
updated_19_K_DC_two <- update(updated_19_K_DC_two, exclu = excl_19_K_DC_two)

widths_19_K_DC_two <- ringWidths(tree_19_K_DC_two, inclu = incl_19_K_DC_two, exclu = excl_19_K_DC_two, last.yr = 2018, ppi = 600)

tree_19_K_DC_three <- ('tree_rings/pictures/19-K_DC_crossSection3.png')
ring_19_K_DC_three <- ringDetect(image = tree_19_K_DC_three, ppi = 600, last.yr = 2018)
incl_19_K_DC_three <- c(60, 180, 340, 409, 499, 622)
excl_19_K_DC_three <- as.numeric(attr(ring_19_K_DC_three, "coln"))[!(as.numeric(attr(ring_19_K_DC_three, "coln")) %in% incl_19_K_DC_three)]
updated_19_K_DC_three <- update(ring_19_K_DC_three, inclu = incl_19_K_DC_three)
updated_19_K_DC_three <- update(updated_19_K_DC_three, exclu = excl_19_K_DC_three)

widths_19_K_DC_three <- ringWidths(tree_19_K_DC_three, inclu = incl_19_K_DC_three, exclu = excl_19_K_DC_three, last.yr = 2018, ppi = 600)

tree_19_K_DC_four <- ('tree_rings/pictures/19-K_DC_crossSection4.png')
ring_19_K_DC_four <- ringDetect(image = tree_19_K_DC_four, segs =1, ppi = 600, last.yr = 2018)
incl_19_K_DC_four <- c(75, 204, 394, 472, 566, 710)
excl_19_K_DC_four <- as.numeric(attr(ring_19_K_DC_four, "coln"))[!(as.numeric(attr(ring_19_K_DC_four, "coln")) %in% incl_19_K_DC_four)]
updated_19_K_DC_four <- update(ring_19_K_DC_four, inclu = incl_19_K_DC_four)
updated_19_K_DC_four <- update(updated_19_K_DC_four, exclu = excl_19_K_DC_four)

widths_19_K_DC_four <- ringWidths(tree_19_K_DC_four, inclu = incl_19_K_DC_four, exclu = excl_19_K_DC_four, last.yr = 2018, ppi = 600)


widths_19_K_DC <- plyr::join_all(list(widths_19_K_DC_one, widths_19_K_DC_two, widths_19_K_DC_three, widths_19_K_DC_four))

widths_19_K_DC <- tidyr::gather(widths_19_K_DC, key = cs_id, value = ring_width_mm, -year)

# export 19_K_DC indv file

write_csv(widths_19_K_DC, path = "tree_rings/data/indv_rings/widths_19_K_DC")

# adding 19_K_DC to master

master <- write_csv(widths_19_K_DC, path = "tree_rings/data/tree_ring_widths_master", append = T)

#### TREE SAMPLE 21-D_AC ####

tree_21_D_AC_one <- ('tree_rings/pictures/21-D_AC_crossSection1.png')
ring_21_D_AC_one <- ringDetect(image = tree_21_D_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_21_D_AC_one <- c(52, 110, 360, 523, 632, 832)
excl_21_D_AC_one <- as.numeric(attr(ring_21_D_AC_one, "coln"))[!(as.numeric(attr(ring_21_D_AC_one, "coln")) %in% incl_21_D_AC_one)]
updated_21_D_AC_one <- update(ring_21_D_AC_one, inclu = incl_21_D_AC_one)
updated_21_D_AC_one <- update(updated_21_D_AC_one, exclu = excl_21_D_AC_one)

widths_21_D_AC_one <- ringWidths(tree_21_D_AC_one, inclu = incl_21_D_AC_one, exclu = excl_21_D_AC_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 21-F_AC ####

tree_21_F_AC_one <- ('tree_rings/pictures/21-F_AC_crossSection1.png')
ring_21_F_AC_one <- ringDetect(image = tree_21_F_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_21_F_AC_one <- c(138, 190, 528, 862, 1069, 1197, 1325)
excl_21_F_AC_one <- as.numeric(attr(ring_21_F_AC_one, "coln"))[!(as.numeric(attr(ring_21_F_AC_one, "coln")) %in% incl_21_F_AC_one)]
updated_21_F_AC_one <- update(ring_21_F_AC_one, inclu = incl_21_F_AC_one)
updated_21_F_AC_one <- update(updated_21_F_AC_one, exclu = excl_21_F_AC_one)

widths_21_F_AC_one <- ringWidths(tree_21_F_AC_one, inclu = incl_21_F_AC_one, exclu = excl_21_F_AC_one, last.yr = 2018, ppi = 600)

# tree_21_F_AC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_21_F_AC_two <- ringDetect(image = tree_21_F_AC_two, ppi = 600, last.yr = 2018)
# incl_21_F_AC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_21_F_AC_two <- as.numeric(attr(ring_21_F_AC_two, "coln"))[!(as.numeric(attr(ring_21_F_AC_two, "coln")) %in% incl_21_F_AC_two)]
# updated_21_F_AC_two <- update(ring_21_F_AC_two, inclu = incl_21_F_AC_two)
# updated_21_F_AC_two <- update(updated_21_F_AC_two, exclu = excl_21_F_AC_two)
# 
# widths_21_F_AC_two <- ringWidths(tree_21_F_AC_two, inclu = incl_21_F_AC_two, exclu = excl_21_F_AC_two, last.yr = 2018, ppi = 600)
# 
# tree_21_F_AC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_21_F_AC_three <- ringDetect(image = tree_21_F_AC_three, ppi = 600, last.yr = 2018)
# incl_21_F_AC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_21_F_AC_three <- as.numeric(attr(ring_21_F_AC_three, "coln"))[!(as.numeric(attr(ring_21_F_AC_three, "coln")) %in% incl_21_F_AC_three)]
# updated_21_F_AC_three <- update(ring_21_F_AC_three, inclu = incl_21_F_AC_three)
# updated_21_F_AC_three <- update(updated_21_F_AC_three, exclu = excl_21_F_AC_three)
# 
# widths_21_F_AC_three <- ringWidths(tree_21_F_AC_three, inclu = incl_21_F_AC_three, exclu = excl_21_F_AC_three, last.yr = 2018, ppi = 600)
# 
# tree_21_F_AC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_21_F_AC_four <- ringDetect(image = tree_21_F_AC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_21_F_AC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_21_F_AC_four <- as.numeric(attr(ring_21_F_AC_four, "coln"))[!(as.numeric(attr(ring_21_F_AC_four, "coln")) %in% incl_21_F_AC_four)]
# updated_21_F_AC_four <- update(ring_21_F_AC_four, inclu = incl_21_F_AC_four)
# updated_21_F_AC_four <- update(updated_21_F_AC_four, exclu = excl_21_F_AC_four)
# 
# widths_21_F_AC_four <- ringWidths(tree_21_F_AC_four, inclu = incl_21_F_AC_four, exclu = excl_21_F_AC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_21_F_AC <- plyr::join_all(list(widths_21_F_AC_one, widths_21_F_AC_two, widths_21_F_AC_three, widths_21_F_AC_four))
# 
# widths_21_F_AC <- tidyr::gather(widths_21_F_AC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_21_F_AC, path = "tree_rings/data/indv_rings/widths_21_F_AC")
# 
# 
# write_csv(widths_21_F_AC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 22-F_DC NOT WORKING ####

# tree_22_F_DC_one <- ('tree_rings/pictures/22-F_DC_crossSection1.png')
# ring_22_F_DC_one <- ringDetect(image = tree_22_F_DC_one, segs =1, ppi = 600, last.yr = 2018)
# incl_22_F_DC_one <- c(150, 166, 397, 570, 641, 837)
# excl_22_F_DC_one <- as.numeric(attr(ring_22_F_DC_one, "coln"))[!(as.numeric(attr(ring_22_F_DC_one, "coln")) %in% incl_22_F_DC_one)]
# updated_22_F_DC_one <- update(ring_22_F_DC_one, inclu = incl_22_F_DC_one)
# updated_22_F_DC_one <- update(updated_22_F_DC_one, exclu = excl_22_F_DC_one)
# 
# widths_22_F_DC_one <- ringWidths(tree_22_F_DC_one, inclu = incl_22_F_DC_one, exclu = excl_22_F_DC_one, last.yr = 2018, ppi = 600)

# tree_22_F_DC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_22_F_DC_two <- ringDetect(image = tree_22_F_DC_two, ppi = 600, last.yr = 2018)
# incl_22_F_DC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_22_F_DC_two <- as.numeric(attr(ring_22_F_DC_two, "coln"))[!(as.numeric(attr(ring_22_F_DC_two, "coln")) %in% incl_22_F_DC_two)]
# updated_22_F_DC_two <- update(ring_22_F_DC_two, inclu = incl_22_F_DC_two)
# updated_22_F_DC_two <- update(updated_22_F_DC_two, exclu = excl_22_F_DC_two)
# 
# widths_22_F_DC_two <- ringWidths(tree_22_F_DC_two, inclu = incl_22_F_DC_two, exclu = excl_22_F_DC_two, last.yr = 2018, ppi = 600)
# 
# tree_22_F_DC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_22_F_DC_three <- ringDetect(image = tree_22_F_DC_three, ppi = 600, last.yr = 2018)
# incl_22_F_DC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_22_F_DC_three <- as.numeric(attr(ring_22_F_DC_three, "coln"))[!(as.numeric(attr(ring_22_F_DC_three, "coln")) %in% incl_22_F_DC_three)]
# updated_22_F_DC_three <- update(ring_22_F_DC_three, inclu = incl_22_F_DC_three)
# updated_22_F_DC_three <- update(updated_22_F_DC_three, exclu = excl_22_F_DC_three)
# 
# widths_22_F_DC_three <- ringWidths(tree_22_F_DC_three, inclu = incl_22_F_DC_three, exclu = excl_22_F_DC_three, last.yr = 2018, ppi = 600)
# 
# tree_22_F_DC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_22_F_DC_four <- ringDetect(image = tree_22_F_DC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_22_F_DC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_22_F_DC_four <- as.numeric(attr(ring_22_F_DC_four, "coln"))[!(as.numeric(attr(ring_22_F_DC_four, "coln")) %in% incl_22_F_DC_four)]
# updated_22_F_DC_four <- update(ring_22_F_DC_four, inclu = incl_22_F_DC_four)
# updated_22_F_DC_four <- update(updated_22_F_DC_four, exclu = excl_22_F_DC_four)
# 
# widths_22_F_DC_four <- ringWidths(tree_22_F_DC_four, inclu = incl_22_F_DC_four, exclu = excl_22_F_DC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_22_F_DC <- plyr::join_all(list(widths_22_F_DC_one, widths_22_F_DC_two, widths_22_F_DC_three, widths_22_F_DC_four))
# 
# widths_22_F_DC <- tidyr::gather(widths_22_F_DC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_22_F_DC, path = "tree_rings/data/indv_rings/widths_22_F_DC")
# 
# 
# write_csv(widths_22_F_DC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 23-H_AN ####

tree_23_H_AN_one <- ('tree_rings/pictures/23-H_AN_crossSection1.png')
ring_23_H_AN_one <- ringDetect(image = tree_23_H_AN_one, segs =1, ppi = 600, last.yr = 2018)
incl_23_H_AN_one <- c(54, 120, 385, 519, 702, 820, 975)
excl_23_H_AN_one <- as.numeric(attr(ring_23_H_AN_one, "coln"))[!(as.numeric(attr(ring_23_H_AN_one, "coln")) %in% incl_23_H_AN_one)]
updated_23_H_AN_one <- update(ring_23_H_AN_one, inclu = incl_23_H_AN_one)
updated_23_H_AN_one <- update(updated_23_H_AN_one, exclu = excl_23_H_AN_one)

widths_23_H_AN_one <- ringWidths(tree_23_H_AN_one, inclu = incl_23_H_AN_one, exclu = excl_23_H_AN_one, last.yr = 2018, ppi = 600)

# tree_23_H_AN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_23_H_AN_two <- ringDetect(image = tree_23_H_AN_two, ppi = 600, last.yr = 2018)
# incl_23_H_AN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_23_H_AN_two <- as.numeric(attr(ring_23_H_AN_two, "coln"))[!(as.numeric(attr(ring_23_H_AN_two, "coln")) %in% incl_23_H_AN_two)]
# updated_23_H_AN_two <- update(ring_23_H_AN_two, inclu = incl_23_H_AN_two)
# updated_23_H_AN_two <- update(updated_23_H_AN_two, exclu = excl_23_H_AN_two)
# 
# widths_23_H_AN_two <- ringWidths(tree_23_H_AN_two, inclu = incl_23_H_AN_two, exclu = excl_23_H_AN_two, last.yr = 2018, ppi = 600)
# 
# tree_23_H_AN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_23_H_AN_three <- ringDetect(image = tree_23_H_AN_three, ppi = 600, last.yr = 2018)
# incl_23_H_AN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_23_H_AN_three <- as.numeric(attr(ring_23_H_AN_three, "coln"))[!(as.numeric(attr(ring_23_H_AN_three, "coln")) %in% incl_23_H_AN_three)]
# updated_23_H_AN_three <- update(ring_23_H_AN_three, inclu = incl_23_H_AN_three)
# updated_23_H_AN_three <- update(updated_23_H_AN_three, exclu = excl_23_H_AN_three)
# 
# widths_23_H_AN_three <- ringWidths(tree_23_H_AN_three, inclu = incl_23_H_AN_three, exclu = excl_23_H_AN_three, last.yr = 2018, ppi = 600)
# 
# tree_23_H_AN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_23_H_AN_four <- ringDetect(image = tree_23_H_AN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_23_H_AN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_23_H_AN_four <- as.numeric(attr(ring_23_H_AN_four, "coln"))[!(as.numeric(attr(ring_23_H_AN_four, "coln")) %in% incl_23_H_AN_four)]
# updated_23_H_AN_four <- update(ring_23_H_AN_four, inclu = incl_23_H_AN_four)
# updated_23_H_AN_four <- update(updated_23_H_AN_four, exclu = excl_23_H_AN_four)
# 
# widths_23_H_AN_four <- ringWidths(tree_23_H_AN_four, inclu = incl_23_H_AN_four, exclu = excl_23_H_AN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_23_H_AN <- plyr::join_all(list(widths_23_H_AN_one, widths_23_H_AN_two, widths_23_H_AN_three, widths_23_H_AN_four))
# 
# widths_23_H_AN <- tidyr::gather(widths_23_H_AN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_23_H_AN, path = "tree_rings/data/indv_rings/widths_23_H_AN")
# 
# 
# write_csv(widths_23_H_AN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 25-Q_DN ####

tree_25_Q_DN_one <- ('tree_rings/pictures/25-Q_DN_crossSection1.png')
ring_25_Q_DN_one <- ringDetect(image = tree_25_Q_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_25_Q_DN_one <- c(176, 265, 447, 582, 700, 879)
excl_25_Q_DN_one <- as.numeric(attr(ring_25_Q_DN_one, "coln"))[!(as.numeric(attr(ring_25_Q_DN_one, "coln")) %in% incl_25_Q_DN_one)]
updated_25_Q_DN_one <- update(ring_25_Q_DN_one, inclu = incl_25_Q_DN_one)
updated_25_Q_DN_one <- update(updated_25_Q_DN_one, exclu = excl_25_Q_DN_one)

widths_25_Q_DN_one <- ringWidths(tree_25_Q_DN_one, inclu = incl_25_Q_DN_one, exclu = excl_25_Q_DN_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 26-B_AN ####

tree_26_B_AN_one <- ('tree_rings/pictures/26-B_AN_crossSection1.png')
ring_26_B_AN_one <- ringDetect(image = tree_26_B_AN_one, segs =1, ppi = 600, last.yr = 2018)
incl_26_B_AN_one <- c(169, 190, 308, 479, 567, 621, 723)
excl_26_B_AN_one <- as.numeric(attr(ring_26_B_AN_one, "coln"))[!(as.numeric(attr(ring_26_B_AN_one, "coln")) %in% incl_26_B_AN_one)]
updated_26_B_AN_one <- update(ring_26_B_AN_one, inclu = incl_26_B_AN_one)
updated_26_B_AN_one <- update(updated_26_B_AN_one, exclu = excl_26_B_AN_one)

widths_26_B_AN_one <- ringWidths(tree_26_B_AN_one, inclu = incl_26_B_AN_one, exclu = excl_26_B_AN_one, last.yr = 2018, ppi = 600)

# tree_26_B_AN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_26_B_AN_two <- ringDetect(image = tree_26_B_AN_two, ppi = 600, last.yr = 2018)
# incl_26_B_AN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_26_B_AN_two <- as.numeric(attr(ring_26_B_AN_two, "coln"))[!(as.numeric(attr(ring_26_B_AN_two, "coln")) %in% incl_26_B_AN_two)]
# updated_26_B_AN_two <- update(ring_26_B_AN_two, inclu = incl_26_B_AN_two)
# updated_26_B_AN_two <- update(updated_26_B_AN_two, exclu = excl_26_B_AN_two)
# 
# widths_26_B_AN_two <- ringWidths(tree_26_B_AN_two, inclu = incl_26_B_AN_two, exclu = excl_26_B_AN_two, last.yr = 2018, ppi = 600)
# 
# tree_26_B_AN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_26_B_AN_three <- ringDetect(image = tree_26_B_AN_three, ppi = 600, last.yr = 2018)
# incl_26_B_AN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_26_B_AN_three <- as.numeric(attr(ring_26_B_AN_three, "coln"))[!(as.numeric(attr(ring_26_B_AN_three, "coln")) %in% incl_26_B_AN_three)]
# updated_26_B_AN_three <- update(ring_26_B_AN_three, inclu = incl_26_B_AN_three)
# updated_26_B_AN_three <- update(updated_26_B_AN_three, exclu = excl_26_B_AN_three)
# 
# widths_26_B_AN_three <- ringWidths(tree_26_B_AN_three, inclu = incl_26_B_AN_three, exclu = excl_26_B_AN_three, last.yr = 2018, ppi = 600)
# 
# tree_26_B_AN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_26_B_AN_four <- ringDetect(image = tree_26_B_AN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_26_B_AN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_26_B_AN_four <- as.numeric(attr(ring_26_B_AN_four, "coln"))[!(as.numeric(attr(ring_26_B_AN_four, "coln")) %in% incl_26_B_AN_four)]
# updated_26_B_AN_four <- update(ring_26_B_AN_four, inclu = incl_26_B_AN_four)
# updated_26_B_AN_four <- update(updated_26_B_AN_four, exclu = excl_26_B_AN_four)
# 
# widths_26_B_AN_four <- ringWidths(tree_26_B_AN_four, inclu = incl_26_B_AN_four, exclu = excl_26_B_AN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_26_B_AN <- plyr::join_all(list(widths_26_B_AN_one, widths_26_B_AN_two, widths_26_B_AN_three, widths_26_B_AN_four))
# 
# widths_26_B_AN <- tidyr::gather(widths_26_B_AN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_26_B_AN, path = "tree_rings/data/indv_rings/widths_26_B_AN")
# 
# 
# write_csv(widths_26_B_AN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 29-B_AC ####

tree_29_B_AC_one <- ('tree_rings/pictures/29-B_AC_crossSection1.png')
ring_29_B_AC_one <- ringDetect(image = tree_29_B_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_29_B_AC_one <- c(93, 154, 408, 661, 845, 975, 1152)
excl_29_B_AC_one <- as.numeric(attr(ring_29_B_AC_one, "coln"))[!(as.numeric(attr(ring_29_B_AC_one, "coln")) %in% incl_29_B_AC_one)]
updated_29_B_AC_one <- update(ring_29_B_AC_one, inclu = incl_29_B_AC_one)
updated_29_B_AC_one <- update(updated_29_B_AC_one, exclu = excl_29_B_AC_one)

widths_29_B_AC_one <- ringWidths(tree_29_B_AC_one, inclu = incl_29_B_AC_one, exclu = excl_29_B_AC_one, last.yr = 2018, ppi = 600)

# tree_29_B_AC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_29_B_AC_two <- ringDetect(image = tree_29_B_AC_two, ppi = 600, last.yr = 2018)
# incl_29_B_AC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_29_B_AC_two <- as.numeric(attr(ring_29_B_AC_two, "coln"))[!(as.numeric(attr(ring_29_B_AC_two, "coln")) %in% incl_29_B_AC_two)]
# updated_29_B_AC_two <- update(ring_29_B_AC_two, inclu = incl_29_B_AC_two)
# updated_29_B_AC_two <- update(updated_29_B_AC_two, exclu = excl_29_B_AC_two)
# 
# widths_29_B_AC_two <- ringWidths(tree_29_B_AC_two, inclu = incl_29_B_AC_two, exclu = excl_29_B_AC_two, last.yr = 2018, ppi = 600)
# 
# tree_29_B_AC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_29_B_AC_three <- ringDetect(image = tree_29_B_AC_three, ppi = 600, last.yr = 2018)
# incl_29_B_AC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_29_B_AC_three <- as.numeric(attr(ring_29_B_AC_three, "coln"))[!(as.numeric(attr(ring_29_B_AC_three, "coln")) %in% incl_29_B_AC_three)]
# updated_29_B_AC_three <- update(ring_29_B_AC_three, inclu = incl_29_B_AC_three)
# updated_29_B_AC_three <- update(updated_29_B_AC_three, exclu = excl_29_B_AC_three)
# 
# widths_29_B_AC_three <- ringWidths(tree_29_B_AC_three, inclu = incl_29_B_AC_three, exclu = excl_29_B_AC_three, last.yr = 2018, ppi = 600)
# 
# tree_29_B_AC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_29_B_AC_four <- ringDetect(image = tree_29_B_AC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_29_B_AC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_29_B_AC_four <- as.numeric(attr(ring_29_B_AC_four, "coln"))[!(as.numeric(attr(ring_29_B_AC_four, "coln")) %in% incl_29_B_AC_four)]
# updated_29_B_AC_four <- update(ring_29_B_AC_four, inclu = incl_29_B_AC_four)
# updated_29_B_AC_four <- update(updated_29_B_AC_four, exclu = excl_29_B_AC_four)
# 
# widths_29_B_AC_four <- ringWidths(tree_29_B_AC_four, inclu = incl_29_B_AC_four, exclu = excl_29_B_AC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_29_B_AC <- plyr::join_all(list(widths_29_B_AC_one, widths_29_B_AC_two, widths_29_B_AC_three, widths_29_B_AC_four))
# 
# widths_29_B_AC <- tidyr::gather(widths_29_B_AC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_29_B_AC, path = "tree_rings/data/indv_rings/widths_29_B_AC")
# 
# 
# write_csv(widths_29_B_AC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 29-N_AC ####

tree_29_N_AC_one <- ('tree_rings/pictures/29-N_AC_crossSection1.png')
ring_29_N_AC_one <- ringDetect(image = tree_29_N_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_29_N_AC_one <- c(132, 210, 513, 680, 803, 909, 1053)
excl_29_N_AC_one <- as.numeric(attr(ring_29_N_AC_one, "coln"))[!(as.numeric(attr(ring_29_N_AC_one, "coln")) %in% incl_29_N_AC_one)]
updated_29_N_AC_one <- update(ring_29_N_AC_one, inclu = incl_29_N_AC_one)
updated_29_N_AC_one <- update(updated_29_N_AC_one, exclu = excl_29_N_AC_one)

widths_29_N_AC_one <- ringWidths(tree_29_N_AC_one, inclu = incl_29_N_AC_one, exclu = excl_29_N_AC_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 30-K_DC ####

tree_30_K_DC_one <- ('tree_rings/pictures/30-K_DC_crossSection1.png')
ring_30_K_DC_one <- ringDetect(image = tree_30_K_DC_one, segs =1, ppi = 600, last.yr = 2018)
incl_30_K_DC_one <- c(93, 121, 232, 391, 572, 811)
excl_30_K_DC_one <- as.numeric(attr(ring_30_K_DC_one, "coln"))[!(as.numeric(attr(ring_30_K_DC_one, "coln")) %in% incl_30_K_DC_one)]
updated_30_K_DC_one <- update(ring_30_K_DC_one, inclu = incl_30_K_DC_one)
updated_30_K_DC_one <- update(updated_30_K_DC_one, exclu = excl_30_K_DC_one)

widths_30_K_DC_one <- ringWidths(tree_30_K_DC_one, inclu = incl_30_K_DC_one, exclu = excl_30_K_DC_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 30-S_DC ####

tree_30_S_DC_one <- ('tree_rings/pictures/30-S_DC_crossSection1.png')
ring_30_S_DC_one <- ringDetect(image = tree_30_S_DC_one, segs =1, ppi = 600, last.yr = 2018)
incl_30_S_DC_one <- c(70, 144, 477, 659, 803, 960)
excl_30_S_DC_one <- as.numeric(attr(ring_30_S_DC_one, "coln"))[!(as.numeric(attr(ring_30_S_DC_one, "coln")) %in% incl_30_S_DC_one)]
updated_30_S_DC_one <- update(ring_30_S_DC_one, inclu = incl_30_S_DC_one)
updated_30_S_DC_one <- update(updated_30_S_DC_one, exclu = excl_30_S_DC_one)

widths_30_S_DC_one <- ringWidths(tree_30_S_DC_one, inclu = incl_30_S_DC_one, exclu = excl_30_S_DC_one, last.yr = 2018, ppi = 600)

# tree_30_S_DC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_30_S_DC_two <- ringDetect(image = tree_30_S_DC_two, ppi = 600, last.yr = 2018)
# incl_30_S_DC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_30_S_DC_two <- as.numeric(attr(ring_30_S_DC_two, "coln"))[!(as.numeric(attr(ring_30_S_DC_two, "coln")) %in% incl_30_S_DC_two)]
# updated_30_S_DC_two <- update(ring_30_S_DC_two, inclu = incl_30_S_DC_two)
# updated_30_S_DC_two <- update(updated_30_S_DC_two, exclu = excl_30_S_DC_two)
# 
# widths_30_S_DC_two <- ringWidths(tree_30_S_DC_two, inclu = incl_30_S_DC_two, exclu = excl_30_S_DC_two, last.yr = 2018, ppi = 600)
# 
# tree_30_S_DC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_30_S_DC_three <- ringDetect(image = tree_30_S_DC_three, ppi = 600, last.yr = 2018)
# incl_30_S_DC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_30_S_DC_three <- as.numeric(attr(ring_30_S_DC_three, "coln"))[!(as.numeric(attr(ring_30_S_DC_three, "coln")) %in% incl_30_S_DC_three)]
# updated_30_S_DC_three <- update(ring_30_S_DC_three, inclu = incl_30_S_DC_three)
# updated_30_S_DC_three <- update(updated_30_S_DC_three, exclu = excl_30_S_DC_three)
# 
# widths_30_S_DC_three <- ringWidths(tree_30_S_DC_three, inclu = incl_30_S_DC_three, exclu = excl_30_S_DC_three, last.yr = 2018, ppi = 600)
# 
# tree_30_S_DC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_30_S_DC_four <- ringDetect(image = tree_30_S_DC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_30_S_DC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_30_S_DC_four <- as.numeric(attr(ring_30_S_DC_four, "coln"))[!(as.numeric(attr(ring_30_S_DC_four, "coln")) %in% incl_30_S_DC_four)]
# updated_30_S_DC_four <- update(ring_30_S_DC_four, inclu = incl_30_S_DC_four)
# updated_30_S_DC_four <- update(updated_30_S_DC_four, exclu = excl_30_S_DC_four)
# 
# widths_30_S_DC_four <- ringWidths(tree_30_S_DC_four, inclu = incl_30_S_DC_four, exclu = excl_30_S_DC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_30_S_DC <- plyr::join_all(list(widths_30_S_DC_one, widths_30_S_DC_two, widths_30_S_DC_three, widths_30_S_DC_four))
# 
# widths_30_S_DC <- tidyr::gather(widths_30_S_DC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_30_S_DC, path = "tree_rings/data/indv_rings/widths_30_S_DC")
# 
# 
# write_csv(widths_30_S_DC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 33-P_DC ####

tree_33_P_DC_one <- ('tree_rings/pictures/33-P_DC_crossSection1.png')
ring_33_P_DC_one <- ringDetect(image = tree_33_P_DC_one, segs =1, ppi = 600, last.yr = 2018)
incl_33_P_DC_one <- c(157, 204, 315, 442, 619, 729)
excl_33_P_DC_one <- as.numeric(attr(ring_33_P_DC_one, "coln"))[!(as.numeric(attr(ring_33_P_DC_one, "coln")) %in% incl_33_P_DC_one)]
updated_33_P_DC_one <- update(ring_33_P_DC_one, inclu = incl_33_P_DC_one)
updated_33_P_DC_one <- update(updated_33_P_DC_one, exclu = excl_33_P_DC_one)

widths_33_P_DC_one <- ringWidths(tree_33_P_DC_one, inclu = incl_33_P_DC_one, exclu = excl_33_P_DC_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 34-Q_AN ####

tree_34_Q_AN_one <- ('tree_rings/pictures/34-Q_AN_crossSection1.png')
ring_34_Q_AN_one <- ringDetect(image = tree_34_Q_AN_one, segs =1, ppi = 600, last.yr = 2018)
incl_34_Q_AN_one <- c(125, 204, 444, 598, 790, 1080)
excl_34_Q_AN_one <- as.numeric(attr(ring_34_Q_AN_one, "coln"))[!(as.numeric(attr(ring_34_Q_AN_one, "coln")) %in% incl_34_Q_AN_one)]
updated_34_Q_AN_one <- update(ring_34_Q_AN_one, inclu = incl_34_Q_AN_one)
updated_34_Q_AN_one <- update(updated_34_Q_AN_one, exclu = excl_34_Q_AN_one)

widths_34_Q_AN_one <- ringWidths(tree_34_Q_AN_one, inclu = incl_34_Q_AN_one, exclu = excl_34_Q_AN_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 35-S_DN ####

tree_35_S_DN_one <- ('tree_rings/pictures/35-S_DN_crossSection1.png')
ring_35_S_DN_one <- ringDetect(image = tree_35_S_DN_one, segs =1, ppi = 600, last.yr = 2018)
incl_35_S_DN_one <- c(160, 190, 265, 427, 546, 725)
excl_35_S_DN_one <- as.numeric(attr(ring_35_S_DN_one, "coln"))[!(as.numeric(attr(ring_35_S_DN_one, "coln")) %in% incl_35_S_DN_one)]
updated_35_S_DN_one <- update(ring_35_S_DN_one, inclu = incl_35_S_DN_one)
updated_35_S_DN_one <- update(updated_35_S_DN_one, exclu = excl_35_S_DN_one)

widths_35_S_DN_one <- ringWidths(tree_35_S_DN_one, inclu = incl_35_S_DN_one, exclu = excl_35_S_DN_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 37-I_AN NOT WORKING ####
# 
# tree_37_I_AN_one <- ('tree_rings/pictures/37-I_AN_crossSection1.png')
# ring_37_I_AN_one <- ringDetect(image = tree_37_I_AN_one, segs =1, ppi = 600, last.yr = 2018)
# incl_37_I_AN_one <- c(150, 166, 397, 570, 641, 837)
# excl_37_I_AN_one <- as.numeric(attr(ring_37_I_AN_one, "coln"))[!(as.numeric(attr(ring_37_I_AN_one, "coln")) %in% incl_37_I_AN_one)]
# updated_37_I_AN_one <- update(ring_37_I_AN_one, inclu = incl_37_I_AN_one)
# updated_37_I_AN_one <- update(updated_37_I_AN_one, exclu = excl_37_I_AN_one)
# 
# widths_37_I_AN_one <- ringWidths(tree_37_I_AN_one, inclu = incl_37_I_AN_one, exclu = excl_37_I_AN_one, last.yr = 2018, ppi = 600)

# tree_37_I_AN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_37_I_AN_two <- ringDetect(image = tree_37_I_AN_two, ppi = 600, last.yr = 2018)
# incl_37_I_AN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_37_I_AN_two <- as.numeric(attr(ring_37_I_AN_two, "coln"))[!(as.numeric(attr(ring_37_I_AN_two, "coln")) %in% incl_37_I_AN_two)]
# updated_37_I_AN_two <- update(ring_37_I_AN_two, inclu = incl_37_I_AN_two)
# updated_37_I_AN_two <- update(updated_37_I_AN_two, exclu = excl_37_I_AN_two)
# 
# widths_37_I_AN_two <- ringWidths(tree_37_I_AN_two, inclu = incl_37_I_AN_two, exclu = excl_37_I_AN_two, last.yr = 2018, ppi = 600)
# 
# tree_37_I_AN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_37_I_AN_three <- ringDetect(image = tree_37_I_AN_three, ppi = 600, last.yr = 2018)
# incl_37_I_AN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_37_I_AN_three <- as.numeric(attr(ring_37_I_AN_three, "coln"))[!(as.numeric(attr(ring_37_I_AN_three, "coln")) %in% incl_37_I_AN_three)]
# updated_37_I_AN_three <- update(ring_37_I_AN_three, inclu = incl_37_I_AN_three)
# updated_37_I_AN_three <- update(updated_37_I_AN_three, exclu = excl_37_I_AN_three)
# 
# widths_37_I_AN_three <- ringWidths(tree_37_I_AN_three, inclu = incl_37_I_AN_three, exclu = excl_37_I_AN_three, last.yr = 2018, ppi = 600)
# 
# tree_37_I_AN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_37_I_AN_four <- ringDetect(image = tree_37_I_AN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_37_I_AN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_37_I_AN_four <- as.numeric(attr(ring_37_I_AN_four, "coln"))[!(as.numeric(attr(ring_37_I_AN_four, "coln")) %in% incl_37_I_AN_four)]
# updated_37_I_AN_four <- update(ring_37_I_AN_four, inclu = incl_37_I_AN_four)
# updated_37_I_AN_four <- update(updated_37_I_AN_four, exclu = excl_37_I_AN_four)
# 
# widths_37_I_AN_four <- ringWidths(tree_37_I_AN_four, inclu = incl_37_I_AN_four, exclu = excl_37_I_AN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_37_I_AN <- plyr::join_all(list(widths_37_I_AN_one, widths_37_I_AN_two, widths_37_I_AN_three, widths_37_I_AN_four))
# 
# widths_37_I_AN <- tidyr::gather(widths_37_I_AN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_37_I_AN, path = "tree_rings/data/indv_rings/widths_37_I_AN")
# 
# 
# write_csv(widths_37_I_AN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 37_J_AN ####

tree_37_J_AN_one <- ('tree_rings/pictures/37-J_AN_crossSection1.png')
ring_37_J_AN_one <- ringDetect(image = tree_37_J_AN_one, segs =1, ppi = 600, last.yr = 2018)
incl_37_J_AN_one <- c(151, 180, 256, 408, 552, 637, 780)
excl_37_J_AN_one <- as.numeric(attr(ring_37_J_AN_one, "coln"))[!(as.numeric(attr(ring_37_J_AN_one, "coln")) %in% incl_37_J_AN_one)]
updated_37_J_AN_one <- update(ring_37_J_AN_one, inclu = incl_37_J_AN_one)
updated_37_J_AN_one <- update(updated_37_J_AN_one, exclu = excl_37_J_AN_one)

widths_37_J_AN_one <- ringWidths(tree_37_J_AN_one, inclu = incl_37_J_AN_one, exclu = excl_37_J_AN_one, last.yr = 2018, ppi = 600)

# tree_37_J_AN_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_37_J_AN_two <- ringDetect(image = tree_37_J_AN_two, ppi = 600, last.yr = 2018)
# incl_37_J_AN_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_37_J_AN_two <- as.numeric(attr(ring_37_J_AN_two, "coln"))[!(as.numeric(attr(ring_37_J_AN_two, "coln")) %in% incl_37_J_AN_two)]
# updated_37_J_AN_two <- update(ring_37_J_AN_two, inclu = incl_37_J_AN_two)
# updated_37_J_AN_two <- update(updated_37_J_AN_two, exclu = excl_37_J_AN_two)
# 
# widths_37_J_AN_two <- ringWidths(tree_37_J_AN_two, inclu = incl_37_J_AN_two, exclu = excl_37_J_AN_two, last.yr = 2018, ppi = 600)
# 
# tree_37_J_AN_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_37_J_AN_three <- ringDetect(image = tree_37_J_AN_three, ppi = 600, last.yr = 2018)
# incl_37_J_AN_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_37_J_AN_three <- as.numeric(attr(ring_37_J_AN_three, "coln"))[!(as.numeric(attr(ring_37_J_AN_three, "coln")) %in% incl_37_J_AN_three)]
# updated_37_J_AN_three <- update(ring_37_J_AN_three, inclu = incl_37_J_AN_three)
# updated_37_J_AN_three <- update(updated_37_J_AN_three, exclu = excl_37_J_AN_three)
# 
# widths_37_J_AN_three <- ringWidths(tree_37_J_AN_three, inclu = incl_37_J_AN_three, exclu = excl_37_J_AN_three, last.yr = 2018, ppi = 600)
# 
# tree_37_J_AN_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_37_J_AN_four <- ringDetect(image = tree_37_J_AN_four, segs =1, ppi = 600, last.yr = 2018)
# incl_37_J_AN_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_37_J_AN_four <- as.numeric(attr(ring_37_J_AN_four, "coln"))[!(as.numeric(attr(ring_37_J_AN_four, "coln")) %in% incl_37_J_AN_four)]
# updated_37_J_AN_four <- update(ring_37_J_AN_four, inclu = incl_37_J_AN_four)
# updated_37_J_AN_four <- update(updated_37_J_AN_four, exclu = excl_37_J_AN_four)
# 
# widths_37_J_AN_four <- ringWidths(tree_37_J_AN_four, inclu = incl_37_J_AN_four, exclu = excl_37_J_AN_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_37_J_AN <- plyr::join_all(list(widths_37_J_AN_one, widths_37_J_AN_two, widths_37_J_AN_three, widths_37_J_AN_four))
# 
# widths_37_J_AN <- tidyr::gather(widths_37_J_AN, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_37_J_AN, path = "tree_rings/data/indv_rings/widths_37_J_AN")
# 
# 
# write_csv(widths_37_J_AN, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 39-B_AC ####

tree_39_B_AC_one <- ('tree_rings/pictures/39-B_AC_crossSection1.png')
ring_39_B_AC_one <- ringDetect(image = tree_39_B_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_39_B_AC_one <- c(72, 181, 364, 481, 578, 790)
excl_39_B_AC_one <- as.numeric(attr(ring_39_B_AC_one, "coln"))[!(as.numeric(attr(ring_39_B_AC_one, "coln")) %in% incl_39_B_AC_one)]
updated_39_B_AC_one <- update(ring_39_B_AC_one, inclu = incl_39_B_AC_one)
updated_39_B_AC_one <- update(updated_39_B_AC_one, exclu = excl_39_B_AC_one)

widths_39_B_AC_one <- ringWidths(tree_39_B_AC_one, inclu = incl_39_B_AC_one, exclu = excl_39_B_AC_one, last.yr = 2018, ppi = 600)

#### TREE SAMPLE 39-P_AC ####

tree_39_P_AC_one <- ('tree_rings/pictures/39-P_AC_crossSection1.png')
ring_39_P_AC_one <- ringDetect(image = tree_39_P_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_39_P_AC_one <- c(82, 150, 331, 534, 689, 784, 923)
excl_39_P_AC_one <- as.numeric(attr(ring_39_P_AC_one, "coln"))[!(as.numeric(attr(ring_39_P_AC_one, "coln")) %in% incl_39_P_AC_one)]
updated_39_P_AC_one <- update(ring_39_P_AC_one, inclu = incl_39_P_AC_one)
updated_39_P_AC_one <- update(updated_39_P_AC_one, exclu = excl_39_P_AC_one)

widths_39_P_AC_one <- ringWidths(tree_39_P_AC_one, inclu = incl_39_P_AC_one, exclu = excl_39_P_AC_one, last.yr = 2018, ppi = 600)

# tree_39_P_AC_two <- ('tree_rings/pictures/2-T_AN_crossSection2.png')
# ring_39_P_AC_two <- ringDetect(image = tree_39_P_AC_two, ppi = 600, last.yr = 2018)
# incl_39_P_AC_two <- c(15, 86, 272, 456, 590, 672, 775)
# excl_39_P_AC_two <- as.numeric(attr(ring_39_P_AC_two, "coln"))[!(as.numeric(attr(ring_39_P_AC_two, "coln")) %in% incl_39_P_AC_two)]
# updated_39_P_AC_two <- update(ring_39_P_AC_two, inclu = incl_39_P_AC_two)
# updated_39_P_AC_two <- update(updated_39_P_AC_two, exclu = excl_39_P_AC_two)
# 
# widths_39_P_AC_two <- ringWidths(tree_39_P_AC_two, inclu = incl_39_P_AC_two, exclu = excl_39_P_AC_two, last.yr = 2018, ppi = 600)
# 
# tree_39_P_AC_three <- ('tree_rings/pictures/2-T_AN_crossSection3.png')
# ring_39_P_AC_three <- ringDetect(image = tree_39_P_AC_three, ppi = 600, last.yr = 2018)
# incl_39_P_AC_three <- c(17, 70, 219, 342, 449, 534, 632)
# excl_39_P_AC_three <- as.numeric(attr(ring_39_P_AC_three, "coln"))[!(as.numeric(attr(ring_39_P_AC_three, "coln")) %in% incl_39_P_AC_three)]
# updated_39_P_AC_three <- update(ring_39_P_AC_three, inclu = incl_39_P_AC_three)
# updated_39_P_AC_three <- update(updated_39_P_AC_three, exclu = excl_39_P_AC_three)
# 
# widths_39_P_AC_three <- ringWidths(tree_39_P_AC_three, inclu = incl_39_P_AC_three, exclu = excl_39_P_AC_three, last.yr = 2018, ppi = 600)
# 
# tree_39_P_AC_four <- ('tree_rings/pictures/2-T_AN_crossSection4.png')
# ring_39_P_AC_four <- ringDetect(image = tree_39_P_AC_four, segs =1, ppi = 600, last.yr = 2018)
# incl_39_P_AC_four <- c(209, 302, 441, 601, 725, 811, 919)
# excl_39_P_AC_four <- as.numeric(attr(ring_39_P_AC_four, "coln"))[!(as.numeric(attr(ring_39_P_AC_four, "coln")) %in% incl_39_P_AC_four)]
# updated_39_P_AC_four <- update(ring_39_P_AC_four, inclu = incl_39_P_AC_four)
# updated_39_P_AC_four <- update(updated_39_P_AC_four, exclu = excl_39_P_AC_four)
# 
# widths_39_P_AC_four <- ringWidths(tree_39_P_AC_four, inclu = incl_39_P_AC_four, exclu = excl_39_P_AC_four, last.yr = 2018, ppi = 600)
# 
# 
# widths_39_P_AC <- plyr::join_all(list(widths_39_P_AC_one, widths_39_P_AC_two, widths_39_P_AC_three, widths_39_P_AC_four))
# 
# widths_39_P_AC <- tidyr::gather(widths_39_P_AC, key = cs_id, value = ring_width_mm, -year)
# 
# write_csv(widths_39_P_AC, path = "tree_rings/data/indv_rings/widths_39_P_AC")
# 
# 
# write_csv(widths_39_P_AC, path = "tree_rings/data/tree_ring_widths_master")

#### TREE SAMPLE 39-S_AC ####

tree_39_S_AC_one <- ('tree_rings/pictures/39-S_AC_crossSection1.png')
ring_39_S_AC_one <- ringDetect(image = tree_39_S_AC_one, segs =1, ppi = 600, last.yr = 2018)
incl_39_S_AC_one <- c(70, 107, 199, 318, 445, 525, 655)
excl_39_S_AC_one <- as.numeric(attr(ring_39_S_AC_one, "coln"))[!(as.numeric(attr(ring_39_S_AC_one, "coln")) %in% incl_39_S_AC_one)]
updated_39_S_AC_one <- update(ring_39_S_AC_one, inclu = incl_39_S_AC_one)
updated_39_S_AC_one <- update(updated_39_S_AC_one, exclu = excl_39_S_AC_one)

widths_39_S_AC_one <- ringWidths(tree_39_S_AC_one, inclu = incl_39_S_AC_one, exclu = excl_39_S_AC_one, last.yr = 2018, ppi = 600)

#### making huuuuuuuge csv of all crossSection1 ####

widths_crossSection1 <- plyr::join_all(list(widths_1_S_DC_one, widths_2_B_AN_one, widths_2_T_AN_one, widths_6_G_DN_one, widths_14_E_DC_one, widths_16_E_DN_one, widths_17_A_DN_one, widths_17_F_DN_one, widths_17_S_DN_one, widths_19_K_DC_one, widths_21_D_AC_one, widths_21_F_AC_one, widths_23_H_AN_one, widths_25_Q_DN_one, widths_26_B_AN_one, widths_29_B_AC_one, widths_29_N_AC_one, widths_30_K_DC_one, widths_30_S_DC_one, widths_33_P_DC_one, widths_34_Q_AN_one, widths_35_S_DN_one, widths_37_J_AN_one, widths_39_B_AC_one, widths_39_P_AC_one, widths_39_S_AC_one))

widths_master_crossSection1 <- tidyr::gather(widths_crossSection1, key = cs_id, value = ring_width_mm, -year)

write_csv(widths_master_crossSection1, path = "tree_rings/data/widths_master_crossSection1.csv")


