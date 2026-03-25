#Discussion figure
library(readxl)
library(tidyverse)

# 1. Data
dat <- read_excel("Data/Table_Discussion.xlsx")

# Names without _
alternative_n <- c(reduce_fertilization= "reduce fertilization",
          riparian_strips = "riparian strips",
          GES = "GES",
          EBFM = "EBFM" ,
          non_invasive_monitoring ="non-invasive monitoring",
          fisheries_monitoring = "fisheries monitoring",
          ICES_advice_system = "ICES advice system",
          reduce_vessels = "reduce vessels",
          new_quota_distribution = "new quota distribution",
          ban_trawling = "ban trawling",
          educational_programs = "educational programs",
          environm_friendly_gear = "environmentally friendly gear",
          searanger = "Sea Ranger",
          subsidies = "subsidies",
          diversification = "diversification",
          re_training = "re-training",
          MPAs = "MPAs",
          no_take_areas = "no take areas",
          promote_trourism = "promote trourism",
          alternative_marketing = "alternative marketing",
          top_predator_reduction = "top predator reduction",
          coastal_infrastructure = "coastal infrastructure")

objective_n <- c(healthy_ecosystem = "healthy ecosystem",
                 protect_stock = "protect stock",
                 s_f_support_livelihoods ="support livelihoods (SF)",
                 safeguard_coastal_pop_well_being = "safeguard well-being",
                 preserve_cultural_value = "preserve cultural value",
                 s_f_economically_viable = "economically viable (SF)",
                 s_f_environmentally_friendly = "environmentally friendly (SF)")

SES_n <- c( S_AS_scientific_advice = "S-AS scientific advice",
           E_AS_predation = "E-AS predation",
           PE_AS_abiotic = "PE-AS abiotic",
           S_AS_mismanagement = "S-AS mismanagement",
           SE_AS_recreational = "SE-AS recreational",
           SE_AS_harvesting = "SE-AS harvesting")

dat$Alternatives <- as.character(alternative_n[dat$Alternatives])
dat$Objectives <- as.character(objective_n[dat$Objectives])
dat$SES_AS <- as.character(SES_n[dat$SES_AS])

#1.Objectives in center----
# 2. Colours
# n <- length(unique(dat$Objectives))
# pal <- unlist(mapply(RColorBrewer::brewer.pal, 9, 'Set1'))
# pal2 <- unlist(mapply(RColorBrewer::brewer.pal, 5, 'Set2'))
# pal3 <- unlist(mapply(RColorBrewer::brewer.pal, 6, 'Set3'))
# pal <- c(pal, pal2, pal3)
# pal <- paste(shQuote(pal[1:n]), collapse = ", ")
# 
# # Domain for Colours
# dom <- unique(dat$Objectives)
# dom <- paste(shQuote(dom), collapse = ", ")
# 
# my_color <- paste0("d3.scaleOrdinal().domain([", dom, ",'nodes']).range([", pal, ",'grey'])")
# 
# 
# # 3. Links
# temp <- dat %>%
#   group_by(Alternatives, Objectives, SES_AS) %>%
#   count()
# 
# # Sub_driver -> Life_history_traits
# links1 <- temp %>%
#   transmute(source = Alternatives, target = Objectives, value = n, linkgroup = Objectives)
# 
# # Life_history_traits -> Live_stage
# links2 <- temp %>%
#   transmute(source = Objectives, target = SES_AS, value = n, linkgroup = Objectives)
# 
# # combine
# sankey_links <- bind_rows(links1, links2)
# 
# # 4. Knots
# nodes <- data.frame(name = unique(c(sankey_links$source, sankey_links$target)))
# nodes$group <- "nodes"
# 
# # IDs
# sankey_links$IDsource <- match(sankey_links$source, nodes$name) - 1
# sankey_links$IDtarget <- match(sankey_links$target, nodes$name) - 1
# 
# # 5. Sankey-Plot
# sankey_traits_plot <- networkD3::sankeyNetwork(
#   Links = sankey_links,
#   Nodes = nodes,
#   Source = "IDsource",
#   Target = "IDtarget",
#   Value = "value",
#   NodeID = "name",
#   fontSize = 14,
#   fontFamily = 'Ubuntu',
#   height = 700,
#   width = 1000,
#   colourScale = my_color,
#   LinkGroup = "linkgroup",
#   NodeGroup = "group"
# )
# 
# # 6. Title
# sankey_traits_plot <- htmlwidgets::prependContent(
#   sankey_traits_plot,
#   htmltools::tags$h5("Pathways in SES")
# )
# 
# sankey_traits_plot
# 
# saveWidget(sankey_traits_plot, "sankey_traits_plot_objectives.html")

#2.SES_AS in center----
# 2. Colours
n <- length(unique(dat$SES_AS))
pal <- unlist(mapply(RColorBrewer::brewer.pal, 9, 'Set1'))
pal2 <- unlist(mapply(RColorBrewer::brewer.pal, 5, 'Set2'))
pal3 <- unlist(mapply(RColorBrewer::brewer.pal, 6, 'Set3'))
pal <- c(pal, pal2, pal3)
pal <- paste(shQuote(pal[1:n]), collapse = ", ")

pal <- c( "'#E41A1C', '#21e3fc', '#FF7F00', '#C1CDCD', '#37fa44'")
#pal <- c( "#E41A1C", '#21e3fc', '#FF7F00', '#C1CDCD', '#37fa44', '#FFFF33')

# Domain for Colours
dom <- unique(dat$SES_AS)
dom <- paste(shQuote(dom), collapse = ", ")

my_color <- paste0("d3.scaleOrdinal().domain([", dom, ",'nodes']).range([", pal, ",'grey'])")
# 3. Links
temp <- dat %>%
  group_by(Alternatives, SES_AS, Objectives) %>%
  count()

# Sub_driver -> Life_history_traits
links1 <- temp %>%
  transmute(source = Alternatives, target = SES_AS, value = n, linkgroup = SES_AS)

# Life_history_traits -> Live_stage
links2 <- temp %>%
  transmute(source = SES_AS, target = Objectives, value = n, linkgroup = SES_AS)

# combine
sankey_links <- bind_rows(links1, links2)

# 4. Knots
nodes <- data.frame(name = unique(c(sankey_links$source, sankey_links$target)))
nodes$group <- "nodes"

# IDs
sankey_links$IDsource <- match(sankey_links$source, nodes$name) - 1
sankey_links$IDtarget <- match(sankey_links$target, nodes$name) - 1

# 5. Sankey-Plot
sankey_traits_plot <- networkD3::sankeyNetwork(
  Links = sankey_links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  fontSize = 18,
  fontFamily = 'Ubuntu',
  height = 700,
  width = 1000,
  colourScale = my_color,
  LinkGroup = "linkgroup",
  NodeGroup = "group"
)

sankey_traits_plot

# 6. Title
# sankey_traits_plot <- htmlwidgets::prependContent(
#   sankey_traits_plot,
#   htmltools::tags$h5("Pathways in SES")
# )

#saveWidget(sankey_traits_plot, "sankey_traits_plot_SES_AS.html")

