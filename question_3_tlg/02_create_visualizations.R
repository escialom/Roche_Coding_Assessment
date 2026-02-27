library(ggplot2)
library(pharmaverseadam)


# Load ADaM datasets from {pharmaverseadam} 
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Get plot 1
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
