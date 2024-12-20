---
title: "Plastic Degradation Term Project"
author:
- Heather MacTavish^1^*
- Andrew Forrest^1^*
- Yvanna Tchatchoua^1^*
- Douglas A. Campbell^1^*
- Mireille Savoie^1^*
- Laurel Genge^1^
- Carlie Barnhill^1^
- Max Berthold^1^

date: "2024-12-04"
output:
  rmdformats::readthedown:
    df_print: paged
    code_folding: hide
    self_contained: true
    keep_md: yes
    fig_caption: yes

bibliography: ../Docs/PBATDegradationCitations.bib
csl:  ../Docs/apa-with-abstract.csl
editor_options: 
  markdown: 
    wrap: 72
---
<style type="text/css">
p.caption {
  font-size: 18px;
}
</style>

# Affiliations {-}
^1^Mount Allison University, New Brunswick, Canada  

*corresponding author 


# Acknowledgement {-}
This R markdown file is adapted from the `Import_MCData` file created by Laurel Genge, Carlie Barnhill, Max Berthold, Mireille Savoie, and Douglas A. Campbell [@genge_import_mcdata_2024] for the Plastic Degradation project by Heather MacTavish, Andrew Forrest, and Yvanna Tchatchoua in BIOL3111.

Dr. Campbell was supported by the Canada Research Chairs.


# Overview

According to Genge et al. (2024):

>This .Rmd Rworkbook imports data in simple .csv long form exported from PSI Multicultivators based upon project specific values for variables set by the user. 

>It tidies and organizes the data. It uses a pivot_wider and interpolation approach to get the Actinic_par and OD values in line rowwise. This requires careful 'arrange' of the rows. It imports a metadata catalog (Data/RawData/"CultureCatalog.Rds") and merges the metadata with the imported data based
upon shared values for the variables 'MC', 'Tube', and 'Filename' which should unambiguously identify a given growth trajectory measured at OD680 or OD720. OD680 of 0.005 and OD720 of 0.003 were used to replace measured OD values for tubes with OD680 and/or OD720 < 0 across the whole growth trajectory. Assigning these very low OD values acts as a place holder; otherwise data for these tubes get filtered out later and 'no growth' data are lost [@genge_import_mcdata_2024].

>It generates preliminary data plots. It filters the data for outliers by screening out values distant from the moving average of a window in the stream; this requires careful 'arrange' of the rows so sequential rows represent sequential time steps [@genge_import_mcdata_2024].

> This works because the OD680 & OD720 data are only episodically, but widely, aberrant when a bubble interrupts the measurement, and if the Multicultivator is running properly these bubble aberration events are rare [@genge_import_mcdata_2024].

This notebook is also used for data visualizations and statistical analyses related to the microbial degradation (synonymous with loss of mass in this context) of vegetable-based bioplastics and for associated chlorophyll analyses.

# Introduction 

Plastics, polymers derived from hydrocarbons and petroleum derivatives, are highly valued for their durability and versatility, making them indispensable across various industries, from packaging to medicine [@buchholz_plastics_2022]. However, these same properties render plastics resistant to degradation in natural environments [@schmaltz_plastic_2020]. As a result, plastics can persist for centuries, fragmenting into microplastics that infiltrate food chains, causing harm to wildlife, ecosystems, and human health[@ziani_microplastics_2023]. Coupled with large-scale global production, this resistance to breakdown has transformed plastic pollution into an escalating global environmental crisis.

In Canada, plastic pollution remains a pressing environmental concern. Approximately 3 million tonnes of plastic waste are discarded annually, with only 9% being recycled. The remaining 91% accumulates in landfills or as litter in natural ecosystems [@canada_plastic_2021]. To address this, Canada has banned several single-use plastics, including straws, stir sticks, and checkout bags, which are challenging to recycle and pose significant risks to wildlife [@canada_plastic_2021].

The natural degradation of plastics by microorganisms provides a potential solution to this crisis [@yang_plastic_2023]. Certain bacteria and fungi produce enzymes such as hydrolases and proteases that can break down polymers into simpler, non-toxic compounds like carbon dioxide, water, or biomass [@munuru_biodegradation_2022]. However, this process is slow and influenced by various factors, including temperature, pH, and oxygen availability, which significantly affect microbial activity and enzyme efficiency [@bacha_biodegradation_2023].

Vegetable starch based plastic, a biodegradable polymer, is increasingly used in applications like flexible packaging and agricultural films due to its ability to degrade under industrial composting conditions [@jian_overview_2020]. It combines the mechanical properties of conventional plastics like Low-density Polyethylene (LDPE) with the added advantage of biodegradability. However, its degradation behavior in natural environments remains poorly understood, as such conditions differ significantly from those in controlled composting facilities[@jian_overview_2020].

Despite the promise of this material and other compostable plastics, questions remain about their environmental performance and interactions with microbial communities [@yang_plastic_2023]. Evidence suggests that incomplete degradation of these materials can lead to the release of microplastics, which may inhibit both autotrophic and heterotrophic bacterial growth due to toxic additives leaching from the particles [@yang_plastic_2023]. Exploring these inhibitory effects and variations in microbial community responses is critical to assessing the sustainability of compostable plastics

This study investigates the degradation of vegetable starch based plastic, sourced from dog poop bags, under two temperature conditions: 15°C and 25°C. It evaluates the reduction of this material by weight under these conditions and examines the polymer’s potential inhibitory effects on microbial growth and diversity.By addressing these gaps, the research contributes valuable insights into the environmental impact of biodegradable plastic , guiding improved waste management practices and informing policy decisions aimed at enhancing the sustainability of biodegradable plastics.

According to Genge et al. (2024):

>The PSI Multicultivator is used to grow 8 x 80 ml of phytoplankton culture under a common temperature regime, with individual control of bubbling, light level, light spectral quality and photoperiod for each of the 8 culture tubes.

**Research Questions**:

- Are compostable plastics truly degradable in simulations of natural environments?
- What factors affect the degradation of the plastic?




# Materials and Methods
## Sampling

At the Mount Allison University swan pond, two samples (1 liter each) of surface water from the pond’s edge at 4553’58’’ N, 6422’12’’ W were collected in sanitized plastic bottles. These samples were then transported back to the lab. 

https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/location_photo.png

![Figure 1. Image of the exact location from which all sampled pond water and microbes within them came from at the Mount Allison swan pond (45°53'58" N 64°22'12" W). Photograph taken by Andrew Forrest using a standard iPhone 11 camera on September 24th, 2024.](https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/location_photo.png?raw=true)

## Preparation of Culture Nutrient

To satisfy minimal nutrient needs and to sustain the microbial community present in pond water samples, nitrate and phosphate solution was prepared. Ingredients included, 2mM disodium phosphate (144L) and 149.6g/L sodium nitrate (12.8mL). Final concentrations of 100M nitrate and 10M phosphate result in the final mixed solution of pond water and nutrient media. Each trial tube contained 80mL of pond water and 0.4mL of nutrient solution.

## Pre-trial Treatment and Weighing of Bioplastic Pieces

A Define Planet Veggie Based Compostable Poobag (SKU: 28946971) was cut into pieces of uniform composition and similar mass. Three holes were then poked through each fragment of plastic. Fragments were then washed in 70% ethanol for 5-10 seconds each to remove any extraneous material. Following ~30 minutes of drying, individual pieces were weighed in an aluminum boat on a Mettler Toledo MX5 microbalance to obtain pre-trial weights of each fragment. At this point fragments were numbered based on the Multi-Cultivator MC 1000-OD photobioreactor (Photon Systems Instruments) tube which they would end up in. Tubes and the plastic fragments they were to contain (if applicable) were labeled 1-16.

![Figure 2. Image of bioplastic fragments with holes punched through. Photograph taken by Andrew Forrest using a standard iPhone 11 camera on November 20th, 2024 (after the trial period had ended).](https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/plastic_photo.png?raw=true)
 
## Photobioreactor Trials

Prior to the addition of nutrient mix or pond water, bag fragments were fastened to glass oxygen delivery tubes within each of the two photobioreactors’ larger tubes. This was done by sliding the folded fragment up the oxygen tube via the holes poked beforehand. Next, 80mL of pond water and 0.4mL of nitrate-phosphate mix were dispensed into each photobioreactor tube (where applicable). Plastic fragments were adjusted to ensure that there was no immediate threat of fragments interfering with the sensors aligned with each tube. The glass housing in which the tubes were located was then filled with distilled water to the point where each reactor’s floating bob was about 1 inch from the low water sensor.

The settings used in photobioreactor 1 and photobioreactor 2 were identical apart from water temperature, which was 15C in photobioreactor 1 and 25C in photobioreactor 2 (meant to mimic water temperature in vivo in summer and fall), . Both bioreactors were set to 12 hours of white light (equal intensity)/12 hours of dark daily, equal oxygen bubbling, and to take measurements at 720nm (optical density) and 680nm (chlorophyll a & b) every 5 minutes. Once these settings had been input into the bioreactor’s software, the photobioreactor was turned on and trials began. 

Beginning the first day of incubation, the contents of each photobioreactor were suspended once weekly. During the trial period, multiple measurements were skewed due to interference of plastic or oxygen delivery tubes with sensors. Such interference issues were fixed as they were noticed by positioning the central oxygen delivery tube and/or the plastic away from sensors. 

## Post-trial Mass Measurement of Bioplastic Pieces

Following a 4-week trial period, the photobioreactors were shut down and plastic fragments were removed from their respective tubes and washed with water to remove any loosely connected mass. Fragments were then left to dry in open air for 24 hours. Post-trial masses of each fragment were taken with the Mettler Toledo MX5 microbalance and an aluminum boat. 

![Figure 3. Image of tray on which plastic fragments were laid out to air dry for 24 hours before post-trial mass measurements were taken and following the end of the trial period. Photograph taken on October 29th, 2024 by Andrew Forrest using a standard iPhone 11 camera.](https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/weighing_plastic_photo.png?raw=true)


## Chlorophyll Assays

To measure wall growth and suspension growth of pond microbes during the trial period, non-acidified chlorophyll assays were performed on the contents of each tube. A total of 32 assays were performed: 16 for suspension growth and 16 for wall growth. Each wall growth assay used 5mL 90% 3:2 acetone: dimethyl sulfoxide solvent to detach microbes from tube walls by swirling. Approximately 2-3mL of solvent/microbe mix was transferred from each tube to its associated assay cuvette. Suspension growth pond water samples were pipetted into assay cuvettes following transfer to a new container and resuspension. Using glass pipettes, 2mL of solvent was distributed to each assay cuvette. Chlorophyll fluorescence readings were taken at excitation wavelength 43610nm and emission wavelength 68510nm using the Turner Trilogy fluorometer.  

![Figure 4. Image of samples following the four week trials, in preperation for chlorophyll assays. Photograph taken on October 29th, 2024 by Andrew Forrest using a standard iPhone 11 camera.](https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/after_bioreact_photo.png?raw=true)

## Data Handling

Formatted display of content from .md file on GitHub site.
Upon knitr figures will be saved to 'Figs/'


### Load Libraries


This report was generated in `R version 4.3.2` [@r_core_team_r_2023], with the packages: `tidyverse`[@wickham_welcome_2019], `lubridate` [@grolemund_dates_2011], `knitr` [@xie_knitr_2023; @xie_dynamic_2015; @stodden_knitr_2014], `broom` [@robinson_broom_2024],`data.table` [@barrett_datatable_2024], and `zoo` [@zeileis_zoo_2005].


``` r
Project <- "Plastic"

# Multicultivator data files are too large to upload in a single .zip therefore 3 .zipped folders are uploaded. 

DataIn <- file.path("..", "Data", "RawData", "MultiCultiPlasticData.zip")

DataOut <- file.path("..","Data", "ProcessedData", "ImportedMCData")

# number of rows to skip upon read csv
Skip <- 20
```



``` r
CultureCatalog <- readRDS(file = file.path("..", "Data",  "RawData","CultureCatalog.Rds"))
```


``` r
#List files in the extracted folder with a ".asc" extension
MCFiles <- unzip(DataIn, list = TRUE)
MCFiles <- MCFiles[grepl(".csv$", MCFiles$Name), "Name"]
```


``` r
TargetFile <- "20240927_Plastic_MCMIX004.csv"

# This opens a compressed file without decompressing.
TargetData <- read_csv(unz(DataIn, TargetFile),  
                       skip = Skip, 
                       id = "Path", 
                       col_names = c("key", "time", 
                                     "abs-time", "value")) %>% 
  separate(col =Path, into = c("Path", "Filename"), sep = ":") 

TargetFileName <- str_remove(string = TargetFile, pattern = ".csv")
```


``` r
#filter superfluous rows to simplify later pivot
TargetData <- TargetData %>%
  filter(str_detect(key, "od-720|od-680|actinic-lights.light")) %>%
  dplyr::select(key, time, `abs-time`, value, Filename) %>% 
  mutate(abs_time = mdy_hms(`abs-time`),
    Tube = str_extract(key, "-[:digit:]"),
         Tube = as.numeric(str_extract(Tube, "[:digit:]"))
         ) %>%
  dplyr::select(-`abs-time`)


#extract StartHour dynamically from first row of abs_time and display for cross check
StartHour <- as.numeric(format(TargetData$abs_time[1], format = "%H"))

 
StartDate <- TargetData$abs_time[1]


#Generate ToD as mod 24 of time + StartHour
TargetData <- TargetData %>%
  mutate(ToD = (time + StartHour) %% 24,
         Day = round((time/24), digits = 0))
#had trouble extracting 'Day' from abs_time, only got 3 unique values
  
#extract ExpDate for matching with Catalog
TargetData <- TargetData %>% 
    mutate(ExpDate = str_extract(Filename, "202[:digit:][:digit:][:digit:][:digit:][:digit:]_"),
           ExpDate = ymd(str_extract(ExpDate, "202[:digit:][:digit:][:digit:][:digit:][:digit:]")))

#extract MC for matching with Catalog
#fixed to run with MC or MCMIX.
#fixed to work for RUN #'s > 100. MC extracted from filename when Runs >99 returns the MC name and RUN #, this removes '_RUN#'
TargetData <- TargetData %>% 
    mutate(MC = str_extract(Filename, "MC.*[:digit:][:digit:][:digit:]"),
           MC = str_remove(MC, pattern = "\\_.*"))
```


# Results and Discussion

## Suprise results!? Does this plastic inhibit microbial growth?

>Plots all OD values. True detection is OD680 and false detection is OD720 [@genge_import_mcdata_2024].


``` r
 TargetData %>%
  filter(grepl("od-", key)) %>%
  ggplot(data = .) +
  geom_point(aes(x = time, y = value, colour = as.factor(str_detect(key, "680"))), size = 0.5) +
   coord_cartesian(ylim = c(-0.08,0.05)) +
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  scale_colour_manual(values = c("black", "red")) +
  labs(y = "Optical Density (OD)", x = "Elapsed Time (h)", title = "Tubes",
       caption = "Figure 5. Optical density (OD) measurements over time at 25°C for tubes with different experimental conditions. Tubes 1–5 \nare replicates containing culture, nutrients, and plastic. Tube 6 contains culture and nutrients but no plastic. Tube 7 \ncontains nutrients and plastic but no culture. Tube 8 contains plastic but no culture or nutrients. True detections (OD680, red) \nand false detections (OD720, black) are shown. Dashed vertical lines represent when resuspension occured at \napproximately 4, 11, 18, 25, and 32 days. The y-axis is constrained to OD values between -0.1 and \n0.1 for clarity.") +
  facet_grid(cols = vars(as.factor(Tube))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, angle = 90), 
        plot.caption = element_text(size = 8, hjust = 0),
        plot.margin = margin(10, 10, 50, 10))
```

![](Figs/prelim plot MCMIX004-1.png)<!-- -->

>Generate par_ue column with rows aligned with OD measures [@genge_import_mcdata_2024]

>Pivot_wider to get actinic-lights data aligned with relevant sensor data. Need to include arrange(Filename, time, Tube) to keep things aligned! Need to group_by and/or reorder rows appropriately; Be Careful [@genge_import_mcdata_2024]


``` r
#possible issue with data structure; there are multiple values for some of the rows of actinic light columns, so the column becomes a list.
#Can add  values_fn = 
#to arbitrarily take the max or min etc. element of the list; but there might be a wider problem here when importing multiple files

TargetDataWide <- TargetData %>%
  pivot_wider(names_from = key, values_from = value, values_fn = list(value = max)) %>%
  arrange(Filename, MC, Tube, time)
rm(TargetData)
```

>Actinic light values do not align time wise with OD measures [@genge_import_mcdata_2024].

>Interpolate NA in actinic light columns from last observation, arrange by MC & Tube Then generate Actinic_par summary column. If multiple lights are activated, this chunk will give the summed par of all different colours for the tube. If a single actinic light is activated per tube, this gives the par for that tube. Filter rows where !is.na(Actinic_par) to check for incorrect row sums [@genge_import_mcdata_2024].

> Interpolation for Sine is not necessarily appropriate interpolation for Square photoregime; issues with propagating last Actinic_par of afternoon through evening, or back-casting first Actinic_par of morning [@genge_import_mcdata_2024].

>Small glitching adding actinic_light values for tubes where actinic_light column should be 0; issue with interpolation we think [@genge_import_mcdata_2024].


``` r
#http://publish.illinois.edu/spencer-guerrero/2014/12/11/2-dealing-with-missing-data-in-r-omit-approx-or-spline-part-1/
#https://dplyr.tidyverse.org/dev/articles/colwise.html
#Interpolation causes problems with final rows that repeat last value.

interpolate <- function(x){zoo::na.locf(x, na.rm = FALSE, fromLast = FALSE, type = "l", maxgap = Inf)}

#possible problem with actinic_par for MC data b/c actinic-lights.light1 = NA generates actinic_par of 0 b/c in rowSums na.rm = TRUE, which treats NA as 0.
#possibly not a big problem but watch for bugs
#na.rm = FALSE fails to run
TargetDataWide <- TargetDataWide %>%
  group_by(Tube) %>%
  arrange(Filename, MC, Tube, time) %>%
  mutate(across(.cols = starts_with("actinic-lights.light"), .fns = interpolate)) %>%
  ungroup() %>%
  mutate(Actinic_par = rowSums(.[grep("actinic-lights.light", names(.))], na.rm = TRUE)) %>%
  filter(!is.na(Actinic_par)) %>%
   dplyr::select(!contains("actinic-lights.light"))
```

>Now that Actinic_par is aligned with each row, coalesce od-sensors-X.od-720 and od-sensors-X.od-680 into 2 columns, b/c 'Tube'is already a column, so no need to identify tube X in od-sensors-X.od-680 columns. This might cause problems later matching OD measures to actinic light colours [@genge_import_mcdata_2024].


``` r
TargetDataWide <- TargetDataWide  %>%
   mutate(OD680 = rowSums(.[grep("od-680", names(.))], na.rm = TRUE),
          OD720 = rowSums(.[grep("od-720", names(.))], na.rm = TRUE)) %>%
   dplyr::select(!contains("od-sensors"))
```


``` r
#This generates 'NA' values for ~1,000,000 rows of 'O2'; possibly the temperature rows?
TargetDataMeta <- left_join(x = TargetDataWide, y= CultureCatalog, by = c("MC", "Tube"))
rm(TargetDataWide)
```

No statistically significant differences in relative fluorescence units were observed (Tables 1–2). However, slight visual trends in the optical density readings (Figures 6–8, 10–12), suggests the need for further testing. Increasing the number of control replicates and extending the observation period may help determine if these trends are consistent and potentially meaningful.


``` r
TargetDataMeta %>% 
  ggplot(aes(x = time, 
             y = OD680,
             colour = as.factor(Tube))) + 
  geom_point(size = 1, alpha = 0.5) + 
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  
  labs( title = "Optical Density Readings Under Different Environmental Conditions", 
        subtitle = "Effect of Plastic, Nutrients, and Culture Presence", x = "Elapsed Time (hours)", 
        y = "Optical Density (OD at 680 nm)", 
        colour = "Tube Number", 
        caption = "Figure 6. Optical Density Readings over time under different environmental conditions 25°C. The plot shows optical density (OD at 680 nm) \nmeasurements for various tube conditions, where 0 indicates the absence and 1 indicates the presence of plastic, \nnutrients, and culture (top-bottom order). Dashed vertical lines represent when resuspension occured at approximately 4, 11, 18, 25, and 32 days. \nThe tubes are grouped by the presence of plastic, nutrients, and culture. Each facet displays different combinations of \nthese factors, with the colour of the points indicating the tube number." ) + 
  facet_wrap(~ Plastic_Present + 
               Nutrient_Present + 
               Culture_Present, 
             ncol = 2) + 
  scale_x_continuous(breaks = seq(0, 800, by = 48)) + 
  scale_y_continuous(limits = c(0, 0.1)) +
  theme_bw() + 
  theme( axis.text.x = element_text(size = 9, angle = 90), 
         strip.text = element_text(size = 10, face = "bold"),
         plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10) )
```

![](Figs/growth different conditions MCMIX004-1.png)<!-- -->


``` r
subset_data <- TargetDataMeta %>%
  filter(Plastic_Present == 1, Nutrient_Present == 1, Culture_Present == 1)

ggplot(subset_data, aes(x = time, 
                        y = OD680, 
                        colour = as.factor(Tube))) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  labs( title = "Optical Density Readings Under Full Environmental Conditions", 
        subtitle = "Plastic, Nutrients, and Culture Present", 
        x = "Elapsed Time (hours)", 
        y = "Optical Density (OD at 680 nm)", 
        colour = "Tube Number", 
        caption = "Figure 7. Optical Density Readings over time with the culture, nutrient, and plastic present at 25°C. The plot shows optical density \n(OD at 680 nm) measurements for a single tube condition. Dashed vertical lines represent when resuspension occured at \napproximately 4, 11, 18, 25, and 32 days." ) + 
  scale_x_continuous(breaks = seq(0, 800, by = 48)) + 
  scale_y_continuous(limits = c(0, 0.1)) +
  theme_bw() + 
  theme( axis.text.x = element_text(size = 9, angle = 90), 
         strip.text = element_text(size = 10, face = "bold"),
         plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10))
```

![](Figs/focusing on the replicates MCMIX004-1.png)<!-- -->


``` r
subset_data <- TargetDataMeta %>%
  filter(Plastic_Present == 0, Nutrient_Present == 1, Culture_Present == 1)

ggplot(subset_data, aes(x = time, 
                        y = OD680, 
                        colour = as.factor(Tube))) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  labs( title = "Optical Density Readings in the Absence of Plastic", 
        subtitle = "Nutrients, and Culture Present, No Plastic", 
        x = "Elapsed Time (hours)", 
        y = "Optical Density (OD at 680 nm)", 
        colour = "Tube Number", 
        caption = "Figure 8. Optical Density Readings over time with the culture and nutrient present, but no plastic at 25°C. The plot shows optical density \n(OD at 680 nm) measurements for a single tube condition. Dashed vertical lines represent when resuspension occured at \napproximately 4, 11, 18, 25, and 32 days." ) + 
  scale_x_continuous(breaks = seq(0, 800, by = 48)) + 
  scale_y_continuous(limits = c(0, 0.1)) +
  scale_colour_manual(values = "#00A9FF") +
  theme_bw() + 
  theme( axis.text.x = element_text(size = 9, angle = 90), 
         strip.text = element_text(size = 10, face = "bold"),
         plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10))
```

![](Figs/focusing on the culture with no plastic MCMIX004-1.png)<!-- -->


``` r
TargetFile <- "20240927_Plastic_MCMIX006.csv"


# This opens a compressed file without decompressing.
TargetData <- read_csv(unz(DataIn, TargetFile),  skip = Skip, id = "Path", col_names = c("key", "time", "abs-time", "value")) %>% 
  separate(col =Path, into = c("Path", "Filename"), sep = ":") 



TargetFileName <- str_remove(string = TargetFile, pattern = ".csv")
```



``` r
#filter superfluous rows to simplify later pivot
TargetData <- TargetData %>%
  filter(str_detect(key, "od-720|od-680|actinic-lights.light"))
#head(TargetData)

# some files have "V5"  instead of "value" column
TargetData <- TargetData %>%
  dplyr::select(key, time, `abs-time`, value, Filename) %>% #, CDateTime
  mutate(Tube = str_extract(key, "-[:digit:]"),
         Tube = as.numeric(str_extract(Tube, "[:digit:]")),
         abs_time = mdy_hms(`abs-time`)) %>%
  dplyr::select(-`abs-time`)


#extract StartHour dynamically from first row of abs_time and display for cross check
StartHour <- as.numeric(format(TargetData$abs_time[1], format = "%H"))

 
StartDate <- TargetData$abs_time[1]

#Generate ToD as mod 24 of time + StartHour
TargetData <- TargetData %>%
  mutate(ToD = (time + StartHour) %% 24,
         Day = round((time/24), digits = 0))
#had trouble extracting 'Day' from abs_time, only got 3 unique values
  
#extract ExpDate for matching with Catalog
TargetData <- TargetData %>% 
    mutate(ExpDate = str_extract(Filename, "202[:digit:][:digit:][:digit:][:digit:][:digit:]_"),
           ExpDate = ymd(str_extract(ExpDate, "202[:digit:][:digit:][:digit:][:digit:][:digit:]")))

#extract MC for matching with Catalog
#fixed to run with MC or MCMIX.
#fixed to work for RUN #'s > 100. MC extracted from filename when Runs >99 returns the MC name and RUN #, this removes '_RUN#'
TargetData <- TargetData %>% 
    mutate(MC = str_extract(Filename, "MC.*[:digit:][:digit:][:digit:]"),
           MC = str_remove(MC, pattern = "\\_.*"))
```

>Plots all OD values. True detection is OD680 and false detection is OD720 [@genge_import_mcdata_2024].


``` r
  TargetData %>%
  filter(grepl("od-", key)) %>%
  ggplot(data = .) +
  geom_point(aes(x = time, y = value, colour = as.factor(str_detect(key, "680"))), size = 0.5) +
   coord_cartesian(ylim = c(-0.08,0.05)) +
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  scale_colour_manual(values = c("black", "red")) +
  labs(y = "Optical Density (OD)", x = "Elapsed Time (h)", title = "Tubes",
       caption = "Figure 9. Optical density (OD) measurements over time at 15°C for tubes with different experimental conditions. Tubes 1–5 \nare replicates containing culture, nutrients, and plastic. Tube 6 contains culture and nutrients but no plastic. Tube 7 \ncontains nutrients and plastic but no culture. Tube 8 contains plastic but no culture or nutrients. True detections (OD680, red) \nand false detections (OD720, black) are shown. Dashed vertical lines represent when resuspension occured at \napproximately 4, 11, 18, 25, and 32 days. The y-axis is constrained to OD values between -0.1 and \n0.1 for clarity.") +
  facet_grid(cols = vars(as.factor(Tube))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, angle = 90), 
        plot.caption = element_text(size = 8, hjust = 0),
        plot.margin = margin(10, 10, 50, 10))
```

![](Figs/prelim plot MCMIX006-1.png)<!-- -->



``` r
#possible issue with data structure; there are multiple values for some of the rows of actinic light columns, so the column becomes a list.
#Can add  values_fn = 
#to arbitrarily take the max or min etc. element of the list; but there might be a wider problem here when importing multiple files

TargetDataWide <- TargetData %>%
  pivot_wider(names_from = key, values_from = value, values_fn = list(value = max)) %>%
  arrange(Filename, MC, Tube, time)

rm(TargetData)
```


``` r
#http://publish.illinois.edu/spencer-guerrero/2014/12/11/2-dealing-with-missing-data-in-r-omit-approx-or-spline-part-1/

#https://dplyr.tidyverse.org/dev/articles/colwise.html

#Interpolation causes problems with final rows that repeat last value.

interpolate <- function(x){zoo::na.locf(x, na.rm = FALSE, fromLast = FALSE, type = "l", maxgap = Inf)}

#possible problem with actinic_par for MC data b/c actinic-lights.light1 = NA generates actinic_par of 0 b/c in rowSums na.rm = TRUE, which treats NA as 0.
#possibly not a big problem but watch for bugs
#na.rm = FALSE fails to run
TargetDataWide <- TargetDataWide %>%
  group_by(Tube) %>%
  arrange(Filename, MC, Tube, time) %>%
  mutate(across(.cols = starts_with("actinic-lights.light"), .fns = interpolate)) %>%
  ungroup() %>%
  mutate(Actinic_par = rowSums(.[grep("actinic-lights.light", names(.))], na.rm = TRUE)) %>%
  filter(!is.na(Actinic_par)) %>%
   dplyr::select(!contains("actinic-lights.light"))
```


``` r
TargetDataWide <- TargetDataWide  %>%
   mutate(OD680 = rowSums(.[grep("od-680", names(.))], na.rm = TRUE),
          OD720 = rowSums(.[grep("od-720", names(.))], na.rm = TRUE)) %>%
   dplyr::select(!contains("od-sensors"))
```


``` r
#This generates 'NA' values for ~1,000,000 rows of 'O2'; possibly the temperature rows?
TargetDataMeta <- left_join(x = TargetDataWide, y= CultureCatalog, by = c("MC", "Tube"))


rm(TargetDataWide)
```



``` r
TargetDataMeta %>% 
  ggplot(aes(x = time, 
             y = OD680,
             colour = as.factor(Tube))) + 
  geom_point(size = 1, alpha = 0.5) + 
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  
  labs( title = "Optical Density Readings Under Different Environmental Conditions", 
        subtitle = "Effect of Plastic, Nutrients, and Culture Presence", x = "Elapsed Time (hours)", 
        y = "Optical Density (OD at 680 nm)", 
        colour = "Tube Number", 
        caption = "Figure 10. Optical Density Readings over time under different environmental conditions. The plot shows optical density (OD at 680 nm) \nmeasurements for various tube conditions at 15°C, where 0 indicates the absence and 1 indicates the presence of plastic, \nnutrients, and culture (top-bottom order). Dashed vertical lines represent when resuspension occured at approximately 4, 11, 18, 25, and 32 days. \nThe tubes are grouped by the presence of plastic, nutrients, and culture. Each facet displays different combinations of \nthese factors, with the colour of the points indicating the tube number." ) + 
  facet_wrap(~ Plastic_Present + 
               Nutrient_Present + 
               Culture_Present, 
             ncol = 2) + 
  scale_x_continuous(breaks = seq(0, 800, by = 48)) + 
  scale_y_continuous(limits = c(0, 0.1)) +
  theme_bw() + 
  theme( axis.text.x = element_text(size = 9, angle = 90), 
         strip.text = element_text(size = 10, face = "bold"),
         plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10) )
```

![](Figs/growth different conditions MCMIX006-1.png)<!-- -->


``` r
subset_data <- TargetDataMeta %>%
  filter(Plastic_Present == 1, Nutrient_Present == 1, Culture_Present == 1)

ggplot(subset_data, aes(x = time, 
                        y = OD680, 
                        colour = as.factor(Tube))) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  labs( title = "Optical Density Readings Under Full Environmental Conditions", 
        subtitle = "Plastic, Nutrients, and Culture Present", 
        x = "Elapsed Time (hours)", 
        y = "Optical Density (OD at 680 nm)", 
        colour = "Tube Number", 
        caption = "Figure 11. Optical Density Readings over time with the culture, nutrient, and plastic present at 15°C. The plot shows optical density \n(OD at 680 nm) measurements for a single tube condition. Dashed vertical lines represent when resuspension occured at \napproximately 4, 11, 18, 25, and 32 days." ) + 
  scale_x_continuous(breaks = seq(0, 800, by = 48)) + 
  scale_y_continuous(limits = c(0, 0.1)) +
  scale_colour_manual(values = c("#00BE67", "#00BFC4","#00A9FF","#C77CFF","#FF61CC")) +
  theme_bw() + 
  theme( axis.text.x = element_text(size = 9, angle = 90), 
         strip.text = element_text(size = 10, face = "bold"),
         plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10))
```

![](Figs/replicates MCMIX006-1.png)<!-- -->


``` r
subset_data <- TargetDataMeta %>%
  filter(Plastic_Present == 0, Nutrient_Present == 1, Culture_Present == 1)

ggplot(subset_data, aes(x = time, 
                        y = OD680, 
                        colour = as.factor(Tube))) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_vline(aes(xintercept = 4.3 * 24), 
             linetype = "dashed") +
  geom_vline(aes(xintercept = 11.3 * 24), 
             linetype = "dashed") + 
  geom_vline(aes(xintercept = 18.3 * 24), 
             linetype =  "dashed") + 
  geom_vline(aes(xintercept = 25.3 * 24), 
             linetype =  "dashed") +
  geom_vline(aes(xintercept = 32.3 * 24), 
             linetype = "dashed") +
  labs( title = "Optical Density Readings in the Absence of Plastic", 
        subtitle = "Nutrients, and Culture Present, No Plastic", 
        x = "Elapsed Time (hours)", 
        y = "Optical Density (OD at 680 nm)", 
        colour = "Tube Number", 
        caption = "Figure 12. Optical Density Readings over time with the culture and nutrient present, but no plastic at 15°C. The plot shows optical density \n(OD at 680 nm) measurements for a single tube condition. Dashed vertical lines represent when resuspension occured at \napproximately 4, 11, 18, 25, and 32 days." ) + 
  scale_x_continuous(breaks = seq(0, 800, by = 48)) + 
  scale_y_continuous(limits = c(0, 0.1)) +
  scale_colour_manual(values = "#7CAE00") +
  theme_bw() + 
  theme( axis.text.x = element_text(size = 9, angle = 90), 
         strip.text = element_text(size = 10, face = "bold"),
         plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10))
```

![](Figs/focusing on the culture with no plastic MCMIX006-1.png)<!-- -->



Table 1. ANOVA to test significance of temperature and plastic presence on relative fluorescence units of wall growth microbes.

``` r
anova_RFU_wall <- aov(Wall_Growth_RFU ~ Plastic_Present * Temperature_C, data = CultureCatalog)
summary(anova_RFU_wall)
```

```
##                               Df  Sum Sq Mean Sq F value Pr(>F)
## Plastic_Present                1  155484  155484   0.245  0.629
## Temperature_C                  1  128169  128169   0.202  0.661
## Plastic_Present:Temperature_C  1  961909  961909   1.517  0.242
## Residuals                     12 7607700  633975
```
Table 2. ANOVA to test significance of temperature and plastic presence on relative fluorescence units of suspended microbes.

``` r
anova_RFU_suspension <- aov(Suspension_RFU ~ Plastic_Present * Temperature_C, data = CultureCatalog)
summary(anova_RFU_suspension)
```

```
##                               Df Sum Sq Mean Sq F value Pr(>F)
## Plastic_Present                1    138   138.2   0.109  0.747
## Temperature_C                  1    948   947.7   0.746  0.405
## Plastic_Present:Temperature_C  1     60    59.7   0.047  0.832
## Residuals                     12  15246  1270.5
```

No statistically significant effect of Plastic Presence, Temperature, or their interaction on RFU measurement in data (p>0.05).


**Impact of Bio-Microplastics on Microbial Communities**

**Degradation of Bioplastics Introduces Microplastics**

- As bioplastics degrade, they release bio-microplastics (bio-MPs) into the environment, impacting microbial ecosystems [@piyathilake_exploring_2024].

**Rapid Increase in Carbon and Altered Chemical Diversity**

- Bio-MPs quickly increase soil dissolved organic carbon (DOC) content due to their degradable nature [@sun_biodegradable_2022].
- Bio-MPs provide abundant carbon (C) but are deficient in nitrogen (N) and phosphorus (P), leading to nutrient imbalances [@karamanlioglu_abiotic_2017].

**Nutrient Deficiency Drives Microbial Competition**

- The lack of nitrogen and phosphorus causes competition among microbes for these limited resources [@cao_organic-c_2021].

**Shifts in Microbial Communities**

- Some microbes thrive by utilizing bio-MPs as a carbon source, promoting their growth [@shi_microplastic_2022].
- Other microbes struggle due to nutrient scarcity, potentially leading to inhibited growth or death [@shi_microplastic_2022].

Possible explanation for more growth at 15 degrees Celsius: Samples were taken from the swan pond near the end of September, when the pond's microbial community may have adjusted to temperatures near 15 degrees Celsius. 



``` r
# make a new data set to not mess up the original
plastic <- CultureCatalog


# calculate percentage change
plastic$perc_change <- (plastic$Final_Plastic_Weight_mg - plastic$Initial_Plastic_Weight_mg) / plastic$Initial_Plastic_Weight_mg * 100
```

## Did the bioplastic fragments lose weight after four weeks?


``` r
# Convert grouping variables to factors 
plastic$Plastic_Present <- as.factor(plastic$Plastic_Present)
plastic$Nutrient_Present <- as.factor(plastic$Nutrient_Present)
plastic$Culture_Present <- as.factor(plastic$Culture_Present)
```


``` r
graph_data <- CultureCatalog %>%
  mutate(Weight_Loss_Percent = ((Initial_Plastic_Weight_mg - Final_Plastic_Weight_mg) / Initial_Plastic_Weight_mg) * 100)

graph_data$Culture_Present <- as.factor(graph_data$Culture_Present)
```


``` r
data_long<-CultureCatalog%>%
  pivot_longer(cols=c(Initial_Plastic_Weight_mg,Final_Plastic_Weight_mg),names_to = "Weight_Type",values_to = "Weight") %>%
  left_join(plastic %>% select(Tube, perc_change), by = "Tube")

#putting them in the right order
data_long$Weight_Type <- factor(data_long$Weight_Type, levels = c("Initial_Plastic_Weight_mg", "Final_Plastic_Weight_mg"))
```


Overall, most of the plastic had a loss of weight after the trial period, except two tubes, which saw an increase in weight (Figure 13), possibly due to folding of plastic causing inability to wash microbes off. The difference in weights was not normal (Table 3), so a Wilcoxon signed-rank test was used and the weight difference was determined to be statistically significant (p < 0.05) (Table 4).


``` r
ggplot(data_long,
       aes(x=Tube,y=Weight,fill=Weight_Type))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(
    values=c("Initial_Plastic_Weight_mg"="lightblue",
             "Final_Plastic_Weight_mg"="red2"))+
  labs(x="Tube",y="Weight(mg)",fill="Weight Type",
    title = "Overall plastic weights (mg) before and after 4 week trial in BioReactors",
       caption = "Figure 13. Bar plot comparing the initial and final plastic weights (mg) across different tubes and temperature conditions. \nThe bars represent the initial and final weights, with initial weights shown in light blue and final weights in red. The plot is \nfaceted by temperature, allowing for the visualization of weight changes at various temperature conditions. For 15°C: \nTube 1 contains plastic but no culture and no nutrient, Tube 2 contains plastic and the nutrient, but no culture, \nTube 3 contains the culture and nutrient, but no plastic, Tubes 4-8 are replicates which contain plastic, nutrient, and \nculture. For 25°C: Tubes 1-5 are replicates which contain plastic, nutrient, and culture, Tube 6 contains the culture \nand nutrient, but no plastic, Tube 7 contains plastic and the nutrient, but no culture, Tube 8 contains plastic \nbut no culture and no nutrient.")+
  facet_wrap(~ Temperature_C, labeller = labeller(Temperature_C = label_both)) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray", color = "black"), 
    strip.text = element_text(size = 9, face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), 
    plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10)) 
```

![](Figs/plotting before and after plastic-1.png)<!-- -->

Table 3. Normality test for difference in weight before and after the trial period.

``` r
CultureCatalog$Weight_Difference_mg <- CultureCatalog$Initial_Plastic_Weight_mg - CultureCatalog$Final_Plastic_Weight_mg

# testing if data is normal
shapiro.test(CultureCatalog$Weight_Difference_mg)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  CultureCatalog$Weight_Difference_mg
## W = 0.74785, p-value = 0.001212
```
Table 4. Wilcox test to determine if the difference in weight before and after the trial is significant.

``` r
wilcox.test(CultureCatalog$Initial_Plastic_Weight_mg, CultureCatalog$Final_Plastic_Weight_mg, paired = TRUE)
```

```
## 
## 	Wilcoxon signed rank exact test
## 
## data:  CultureCatalog$Initial_Plastic_Weight_mg and CultureCatalog$Final_Plastic_Weight_mg
## V = 90, p-value = 0.0166
## alternative hypothesis: true location shift is not equal to 0
```






## What factors influence the magnitude of change in bioplastic fragment weight?


``` r
# Add a column for Percent Weight Loss
plastic <- plastic %>%
  mutate(
    Percent_Weight_Loss = ((Initial_Plastic_Weight_mg - Final_Plastic_Weight_mg) / Initial_Plastic_Weight_mg) * 100
  )
plastic <- plastic %>%
  mutate(
    Temperature_C = as.factor(Temperature_C),
    Culture_Present = as.factor(Culture_Present),
    Nutrient_Present = as.factor(Nutrient_Present)
  )
```


``` r
summary_stats <- plastic %>%
  group_by(Temperature_C, Culture_Present, Nutrient_Present) %>%
  summarise(
    mean_loss = mean(Percent_Weight_Loss, na.rm = TRUE),
    se_loss = sd(Percent_Weight_Loss, na.rm = TRUE) / sqrt(n()),
    .groups = "drop")

# mean loss having negative means that it actually grew
```



None of the manipulated factors or their interactions show a statistically significant effect on the percentage change in mass within our dataset (Table 5). This lack of statistical significance indicates that culture presence, nutrient presence, and temperature do not have a measurable impact on the outcome in this experiment. However, slight visual trends (Figure 14) suggest the potential for differences, and further testing with additional replicates of the control tubes, is needed to determine if these visual differences are consistent and potentially meaningful.


``` r
# Bar Plot with Facet Labels and Borders
ggplot(summary_stats, aes(x = factor(Temperature_C), y = mean_loss, fill = Culture_Present)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_loss - se_loss, ymax = mean_loss + se_loss),
                position = position_dodge(0.9), width = 0.2) +
  labs(
    x = "Temperature (°C)",
    y = "Mean Percent Weight Loss",
    fill = "Culture Present",
    title = "Average percent weight loss of plastic",
    caption = "Figure 14. Bar plot showing the mean percent weight loss across different temperature conditions, with error \nbars representing the standard error of the mean.  The plot is grouped by Culture Presence (indicated by colour) \nand faceted by Nutrient Presence. Temperature is shown on the x-axis, and the mean percent weight loss \nis plotted on the y-axis. Facet labels distinguish the presence or absence of nutrients in the experiment."
  ) +
  facet_wrap(~ Nutrient_Present, labeller = labeller(Nutrient_Present = label_both)) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray", color = "black"), 
    strip.text = element_text(size = 12, face = "bold"), # Style facet labels
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), , 
    plot.caption = element_text(size = 8, hjust = 0),
         plot.margin = margin(10, 10, 50, 10) 
  )
```

![](Figs/unnamed-chunk-4-1.png)<!-- -->

Table 5. ANOVA testing the effects of Culture Presence, Nutrient Presence, and Temperature on the percentage change in the outcome variable as well as their interactions.


``` r
anova_effects <- aov(Percent_Weight_Loss ~ Culture_Present * Nutrient_Present * Temperature_C, data = plastic)
summary(anova_effects)
```

```
##                                Df Sum Sq Mean Sq F value Pr(>F)
## Culture_Present                 1    3.0    3.03   0.029  0.869
## Nutrient_Present                1    0.8    0.79   0.008  0.933
## Temperature_C                   1  200.4  200.41   1.912  0.204
## Culture_Present:Temperature_C   1  127.1  127.11   1.213  0.303
## Nutrient_Present:Temperature_C  1    0.1    0.09   0.001  0.978
## Residuals                       8  838.5  104.81               
## 2 observations deleted due to missingness
```

## Microbes Present 

The 15°C bioreactor had few microorganisms, contained *Euglena*, diatoms, and *Scenedesmus* (Figure 15). It was dominated by *Euglena*.





![Figure 15. *Scenedesmus* (a, b), *Euglena* (c), and Diatom (d) from 15 degree celsius bioreactor (MC-Mix-006) control tube 3 (plastic omitted) viewed under a light microscope at 400x magnification.](https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/Microbes%20bioreactor%2015%20degrees.png?raw=true)

The 25°C bioreactor had more organisms than the 15°C bioreactor and was dominated by *Scenedesmus* (Figure 16).




![Figure 16. *Scenedesmus* (a, b,c,d) for 25 degree Celsius bioreactor (MC-Mix-004) control tube 6 (plastic omitted) viewed under an light microscope at 400x magnification.](https://github.com/BIOL3111MtA2024/plastic_degrade/blob/main/Docs/Photos/Microbes%20bioreactor%2025%20degrees.png?raw=true)



## Limitations

- Duration of the experiment was relatively short. 
- Photobioreactor issues and inability to correct them.
- No way to confirm equal provision of oxygen.
- Lacked a mechanism to control plastic fragment folding and exposure to physical stresses. 

## Future Direction

- Extend experiment duration. 
- Use oxygen sensors to ensure equal exposure.
- Discover and use community's optimal nutrient quantities.
- Add replicates for control tube conditions.
- Testing bioplastic microbial growth inhibition: A future study to investigate the potential role of growth inhibition by vegetable-based bioplastics and/or their breakdown products could use multiple identical bioreactors to test the ability of a variety of naturally occurring microbial communities to grow in the presence of gradually increasing masses of vegetable starch based plastic fragments. With an accurate way to periodically measure total biomass in each tube, it's possible that a conclusion could be provided regarding whether or not these bioplastics' presence inhibit the growth of various microbial communities.

# Conclusion

Overall, although changes in weight were observed before and after the trial, no statistically significant effects were found from the manipulated factors on weight change. Similarly, no significant differences in relative fluorescence units (RFUs) were observed, indicating that the tested factors had no apparent effect on RFU levels. Further investigation is needed to explore these factors in more depth.

# References


