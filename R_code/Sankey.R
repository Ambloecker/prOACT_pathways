# Discussion figure
library(readxl)
library(tidyverse)
library(networkD3)
# 1. Data
dat <- read_excel("Data/Sankey_diagram_table.xlsx")
# Names without _
measures_n <- c(reduce_fertilization= "Reduce fertilization",
                riparian_strips = "Riparian strips",
                GES = "GES",
                EBFM = "EBFM" ,
                non_invasive_monitoring ="Non-invasive monitoring",
                fisheries_monitoring = "Fisheries monitoring",
                ICES_advice_system = "ICES advice system",
                reduce_vessels = "Reduce vessels",
                new_quota_distribution = "New quota distribution",
                ban_trawling = "Ban trawling",
                educational_programs = "Educational programs",
                environm_friendly_gear = "Environmentally friendly gear",
                searanger = "Sea Ranger",
                subsidies = "Subsidies",
                diversification = "Diversification",
                re_training = "Re-training",
                MPAs = "MPAs",
                no_take_areas = "No take areas",
                promote_trourism = "Promote tourism",
                alternative_marketing = "Alternative marketing",
                top_predator_reduction = "Top predator reduction",
                coastal_infrastructure = "Coastal infrastructure")
objective_n <- c(healthy_ecosystem = "Healthy ecosystem",
                 protect_stock = "Protect stock",
                 s_f_support_livelihoods ="Support livelihoods (SF)",
                 safeguard_coastal_pop_well_being = "Safeguard well-being",
                 preserve_cultural_value = "Preserve cultural value",
                 s_f_economically_viable = "Economically viable (SF)",
                 s_f_environmentally_friendly = "Environmentally friendly (SF)")
SES_n <- c( S_AS_scientific_advice = "S-AS scientific advice",
            E_AS_predation = "E-AS predation",
            PE_AS_abiotic = "PE-AS abiotic",
            S_AS_mismanagement = "S-AS mismanagement",
            SE_AS_recreational = "SE-AS recreational",
            SE_AS_harvesting = "SE-AS harvesting")
dat$Measures <- as.character(measures_n[dat$Measures])
dat$Objectives <- as.character(objective_n[dat$Objectives])
dat$SES_AS <- as.character(SES_n[dat$SES_AS])

#2.SES_AS in center----
# 2. Colours
# Define color mapping with underscores to prevent networkD3 JS truncation
ses_colors <- c(
  "SE-AS_harvesting"       = "#B82E72",  # orange
  "SE-AS_recreational"     = "#B82E72",  # orange
  "S-AS_scientific_advice" = "#7a37b8",  # purple
  "PE-AS_abiotic"          = "#C1CDCD",  # grey
  "E-AS_predation"         = "#21e3fc",  # blue
  "S-AS_mismanagement"     = "#E41A1C"   # red
)

# 3. Links
temp <- dat %>%
  group_by(Measures, SES_AS, Objectives) %>%
  count()
# Sub_driver -> Life_history_traits
links1 <- temp %>%
  transmute(source = Measures, target = SES_AS, value = n, linkgroup = SES_AS)
# Life_history_traits -> Live_stage
links2 <- temp %>%
  transmute(source = SES_AS, target = Objectives, value = n, linkgroup = SES_AS)
# combine
sankey_links <- bind_rows(links1, links2)
# Replace spaces in linkgroup with underscores to avoid JS truncation!
sankey_links$linkgroup <- gsub(" ", "_", sankey_links$linkgroup)

# Generate DOM and PAL matching the modified link groups
dom <- unique(sankey_links$linkgroup)
dom <- dom[!is.na(dom)]
pal <- sapply(dom, function(x) {
  if (x %in% names(ses_colors)) {
    ses_colors[x]
  } else {
    "#E41A1C" # default for any "others"
  }
})
dom_str <- paste(shQuote(dom), collapse = ", ")
pal_str <- paste(shQuote(pal), collapse = ", ")
my_color <- paste0(
  "d3.scaleOrdinal().domain([", dom_str, ",'nodes']).range([", pal_str, ",'grey'])"
)

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
