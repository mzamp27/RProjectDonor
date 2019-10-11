# Four growth metrics.
# 
# 1.Donor count
# 2.Donation total
# 3.Donor growth rate
# 4.Donation growth rate

donor_count19 <- length(which(donor$FY19 != 0))
donor_count18 <- length(which(donor$FY18 != 0))
donor_count17 <- length(which(donor$FY17 != 0))
donor_count16 <- length(which(donor$FY16 != 0))
donor_count15 <- length(which(donor$FY15 != 0))
donor_countVEC <- c(donor_count19,donor_count18,donor_count17,donor_count16,donor_count15)


donation_total19 <- sum(donor$FY19)
donation_total18 <- sum(donor$FY18)
donation_total17 <- sum(donor$FY17)
donation_total16 <- sum(donor$FY16)
donation_total15 <- sum(donor$FY15)
donation_totalVEC <- c(donation_total19,donation_total18,donation_total17,donation_total16,donation_total15)


years <- c(2019,2018,2017,2016,2015)


#donor count change YOY
donor_count_19_vs_18 <- donor_count19 - donor_count18 
donor_count_18_vs_17 <- donor_count18 - donor_count17
donor_count_17_vs_16 <- donor_count18 - donor_count17
donor_count_16_vs_15 <- donor_count18 - donor_count17
donor_count_15_vs_14 <- NA
donor_count_changeVEC <- c(donor_count_19_vs_18,donor_count_18_vs_17,donor_count_17_vs_16,donor_count_16_vs_15,donor_count_15_vs_14)


# total donations change $ YoY 
donation_sum_19_vs_18 <- donation_total19  - donation_total18 #(INCREASE of $452,535)
donation_sum_18_vs_17 <- donation_total18  - donation_total17
donation_sum_17_vs_16 <- donation_total17  - donation_total16
donation_sum_16_vs_15 <- donation_total16  - donation_total15
donation_sum_15_vs_14 <- NA
donation_sum_changeVEC <- c(donation_sum_19_vs_18,donation_sum_18_vs_17,donation_sum_17_vs_16,donation_sum_16_vs_15,donation_sum_15_vs_14)

#donor growth rate (Most Recent DECREASE of  -4.5%)
donor_growth_rate_19_vs_18 <- (donor_count19 - donor_count18) / donor_count18  * 100
donor_growth_rate_18_vs_17 <- (donor_count18 - donor_count17) / donor_count17  * 100
donor_growth_rate_17_vs_16 <- (donor_count17 - donor_count16) / donor_count16  * 100
donor_growth_rate_16_vs_15 <- (donor_count16 - donor_count15) / donor_count15  * 100
donor_growth_rate_15_vs_14 <- NA
donor_growth_rateVEC <- c(donor_growth_rate_19_vs_18,donor_growth_rate_18_vs_17,donor_growth_rate_17_vs_16,donor_growth_rate_16_vs_15,donor_growth_rate_15_vs_14)

#donation growth rate (Most Recent INCREASE of 1.9% )
donation_growth_rate_19_vs_18 <- (donation_total19 - donation_total18) / donation_total18  * 100
donation_growth_rate_18_vs_17 <- (donation_total18 - donation_total17) / donation_total17  * 100
donation_growth_rate_17_vs_16 <- (donation_total17 - donation_total16) / donation_total16  * 100
donation_growth_rate_16_vs_15 <- (donation_total16 - donation_total15) / donation_total15  * 100
donation_growth_rate_15_vs_14 <- NA
donation_growth_rateVEC <- c(donation_growth_rate_19_vs_18,donation_growth_rate_18_vs_17,donation_growth_rate_17_vs_16,donation_growth_rate_16_vs_15,donation_growth_rate_15_vs_14)



#Data table to contain all relevant information
DF <- data.frame(Year = years, Donor_Count = donor_countVEC, Donor_Change = donor_count_changeVEC,
                 Donation_Total = donation_totalVEC, Donation_Change = donation_sum_changeVEC,
                 Donor_Growth_Rate = donor_growth_rateVEC, Donation_Growth_Rate = donation_growth_rateVEC)



#Simple Graph examples
DF %>% ggplot(aes(Year,Donor_Count, label = Donor_Count)) + geom_line() + geom_point() + geom_label_repel() + labs(x ="Year",y="Donor Count",title = "Donor Counts Past 5 Years")

DF %>% ggplot(aes(Year,Donation_Growth_Rate)) + geom_bar(aes(fill = Donation_Growth_Rate < 0), stat = "identity") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "red"))


