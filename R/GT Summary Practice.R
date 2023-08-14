library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")


###Practice###

#1. Download the R script here and put it into your in-class project folder.


#2. Install {gtsummary} and run the examples.


#3. Make a tbl_summary(). Include categorical region, race/ethnicity, income,
#and the sleep variables (use a helper function to select those) and
#make sure they are nicely labeled.

tbl_summary(
	nlsy,
	include = c(contains(c("sleep","income")),
							contains(c("race","region")) & ends_with("cat")),
	label = list(
		sleep_wkdy ~ "Weekday Sleep",
		sleep_wknd ~ "Weekend Sleep",
		income ~ "Income",
		race_eth_cat ~ "Race/Ethnicity",
		region_cat ~ "Region"
	)
)


#4. Stratify the table by sex. Add a p-value comparing the sexes and an overall
#column combining both sexes.

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(contains(c("sleep","income")),
							contains(c("race","region")) & ends_with("cat")),
	label = list(
		sleep_wkdy ~ "Weekday Sleep",
		sleep_wknd ~ "Weekend Sleep",
		income ~ "Income",
		race_eth_cat ~ "Race/Ethnicity",
		region_cat ~ "Region"
	)
) |>
	add_p(test = list(all_continuous()~"t.test",
										all_categorical()~"chisq.test"))


#5. For the income variable, show the 10th and 90th percentiles of income with 3
#digits, and for the sleep variables, show the min and the max with 1 digit.

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(contains(c("sleep","income")),
							contains(c("race","region")) & ends_with("cat")),
	label = list(
		sleep_wkdy ~ "Weekday Sleep",
		sleep_wknd ~ "Weekend Sleep",
		income ~ "Income",
		race_eth_cat ~ "Race/Ethnicity",
		region_cat ~ "Region"
	),
	statistic = list(income ~ "10th {p10}, 90th {p90}",
									 sleep_wkdy ~"Min = {min}, Max = {max}",
									 sleep_wknd ~"Min = {min}, Max = {max}"
	),
	digits = list(income ~ c(3,3),
								sleep_wkdy ~ c(1,1),
								sleep_wknd ~ c(1,1)),
) |>
	add_p(test = list(all_continuous()~"t.test",
										all_categorical()~"chisq.test"))


#6. Add a footnote to the race/ethnicity variable with a link to the page describing
#how NLSY classified participants:
#https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(contains(c("sleep","income")),
							contains(c("race","region")) & ends_with("cat")),
	label = list(
		sleep_wkdy ~ "Weekday Sleep",
		sleep_wknd ~ "Weekend Sleep",
		income ~ "Income",
		race_eth_cat ~ "Race/Ethnicity",
		region_cat ~ "Region"
	),
	statistic = list(income ~ "10th {p10}, 90th {p90}",
									 sleep_wkdy ~"Min = {min}, Max = {max}",
									 sleep_wknd ~"Min = {min}, Max = {max}"
	),
	digits = list(income ~ c(3,3),
								sleep_wkdy ~ c(1,1),
								sleep_wknd ~ c(1,1)),
) |>
	add_p(test = list(all_continuous()~"t.test",
										all_categorical()~"chisq.test")) |>
	modify_table_styling(
		column = label,
		rows = label == "Race/Ethnicity",
		footnote= "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
		)


#7. Play around with the extra functions from the examples and/or from the documentation

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(contains(c("sleep","income")),
							contains(c("race","region")) & ends_with("cat")),
	label = list(
		sleep_wkdy ~ "Weekday Sleep",
		sleep_wknd ~ "Weekend Sleep",
		income ~ "Income",
		race_eth_cat ~ "Race/Ethnicity",
		region_cat ~ "Region"
							),
	statistic = list(income ~ "10th {p10}, 90th {p90}",
									 sleep_wkdy ~"Min = {min}, Max = {max}",
									 sleep_wknd ~"Min = {min}, Max = {max}"
									 ),
	digits = list(income ~ c(3,3),
								sleep_wkdy ~ c(1,1),
								sleep_wknd ~ c(1,1)),
	missing_text = "Missing"
	) |>
	add_p(test = list(all_continuous()~"t.test",
										all_categorical()~"chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_table_styling(
		column = label,
		rows = label == "Race/Ethnicity",
		footnote= "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
	)|>
	modify_footnote(update = )
	modify_header(
		label = "**Variable**",
		p.value = "**P**"
	)

