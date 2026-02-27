library(ggplot2)
library(pharmaverseadam)
library(purrr)


# Load ADaM datasets from {pharmaverseadam} 
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl


#### Get plot 1 ####

plt1 <- adae %>%
  filter(TRTEMFL == "Y") %>%
  count(ACTARM, AESEV) %>%
  ggplot(aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_col() +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme(axis.text.x = element_text(size = 6))

# Save plot 1
ggsave("question_3_tlg/plot1.png", plot = plt1)


#### Get plot 2 ####

# Get the total number of subjects to calculate percentage
N.subj <- length(unique(adae$USUBJID))

# Get the percentages of AE and their confidence interval
ae.stats <- adae %>%
  distinct(USUBJID, AEDECOD) %>%   
  group_by(AEDECOD) %>%
  summarize(
    n = n(),                      
    percent = n / N.subj * 100,
    # For each AE term, run an exact binomial test:
    # Given n events out of N subjects, what is the exact 95% confidence interval for the true probability p?
    # We use the mapping to apply the binomial test to each n
    # .x is the syntax to use to denote the current n
    bt = map(n, ~binom.test(.x, N.subj, conf.level = 0.95)),
    # Extract the lower CI bound from each binom.test result and convert to %
    # Use map_dbl to ensure numeric output
    lower = map_dbl(bt, ~.x$conf.int[1] * 100),
    # Extract the upper CI bound from each binom.test result and convert to %
    upper = map_dbl(bt, ~.x$conf.int[2] * 100)
  )%>%
  arrange(desc(percent)) %>%
  slice_head(n = 10)

# Get plot 2
# We need to reorder the y axis to prevent ggplot from taking the order from 
# the factor level
plt2 <- ggplot(ae.stats, aes(x = percent, y = reorder(AEDECOD, percent))) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point() +
  # Get the percent on the x ticks
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N.subj, " subjects; 95% Clopper Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  )

# Save plot 2
ggsave("question_3_tlg/plot2.png", plot = plt2)



