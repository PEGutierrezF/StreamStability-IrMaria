


physico <- read.xlsx("data/data_cfa.xlsx", sheet='Physico_QPA',
                     detectDates = TRUE)
head(physico)
summary(physico)


# Extract year and month
physico_QPA <- physico %>%
  mutate(Year = lubridate::year(Sample_Date),
         Month = lubridate::month(Sample_Date))

# Group by Year and Month and calculate the mean
phys_QPA <- physico_QPA %>%
  group_by(Year, Month) %>%
  summarise(across(c(Temp, Potassium, Nitrate), mean, na.rm = TRUE))

# Remove the first 9 rows
phys_QPA <- phys_QPA[-(1:8), ]

print(phys_QPA)


