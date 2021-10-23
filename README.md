# Financial-Analysis
Contains my Financial Analysis codes

## Set path (make sure that your SharePoint is synchornized to your local exporer window;
## then you can just exchange the n number)

wd <- setwd("~/MortgageTrend")
wd
## Check if all libraries are installed

packages = c("quantmod", "zoo", "lubridate", "tsibble", "readxl", "ggplot2", "dplyr", "margins", "glmnet",
             "forecast", "tidyverse", "tidyr", "rlist", "boot", "ggpubr", "anytime", "Rcpp"
)

# Install package if required, load package
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

histStartDt <- ymd("20000101")
histEndDt <- ymd("20210101")

## to be automatised is the household debt to income ratio:
## https://www.federalreserve.gov/releases/z1/dataviz/household_debt/state/map/#year:2019

## load data and set names
pathSTPBExcl <- "~/MortgageTrend/state_level_unemployment_statistics.xlsx"

indices_raw <- read_excel(pathSTPBExcl,sheet="source",skip=0)

AllTransHousePriceIndex <- indices_raw$AllTransHousePriceIndex
HomeVacancyRate <- indices_raw$HomeVacancyRate
HomeownershipRate <- indices_raw$HomeownershipRate
RentalVacancyRate <- indices_raw$RentalVacancyRate
RealEstateRentalLeasingEarnings <- indices_raw$RealEstateRentalLeasingEarnings
UnemploymentRate <- indices_raw$UnemploymentRate
GDPTotal <- indices_raw$GDPTotal
BuildingPermits <- indices_raw$BuildingPermits
MinimumWage <- indices_raw$MinimumWage

table(AllTransHousePriceIndex)

# quarterly
AllTransHousePriceIndex_df <- NULL
#idx <- 1
for (idx in seq(length(AllTransHousePriceIndex))){
  houseprices_index <- AllTransHousePriceIndex[idx]
  temp_df <- as.data.frame(getSymbols(houseprices_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = houseprices_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("HousePriceIndex", "Date", "Series")
  AllTransHousePriceIndex_df = rbind(AllTransHousePriceIndex_df, temp_df)
}
AllTransHousePriceIndex_df <- AllTransHousePriceIndex_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date),
         quarter=yearquarter(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","AllTransHousePriceIndex")],by=c("Series"="AllTransHousePriceIndex")) %>%
  select(-c("Series"))


# yearly
HomeVacancyRate_df <- NULL
#idx <- 1
for (idx in seq(length(HomeVacancyRate))){
  HomeVacancyRate_index <- HomeVacancyRate[idx]
  temp_df <- as.data.frame(getSymbols(HomeVacancyRate_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = HomeVacancyRate_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("HomeVacancyRate", "Date", "Series")
  HomeVacancyRate_df = rbind(HomeVacancyRate_df, temp_df)
}

HomeVacancyRate_df <- HomeVacancyRate_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","HomeVacancyRate")],by=c("Series"="HomeVacancyRate")) %>%
  select(-c("Series","Date"))



# yearly
RentalVacancyRate_df <- NULL
#idx <- 1
for (idx in seq(length(RentalVacancyRate))){
  RentalVacancyRate_index <- RentalVacancyRate[idx]
  temp_df <- as.data.frame(getSymbols(RentalVacancyRate_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = RentalVacancyRate_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("RentalVacancyRate", "Date", "Series")
  RentalVacancyRate_df = rbind(RentalVacancyRate_df, temp_df)
}

RentalVacancyRate_df <- RentalVacancyRate_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","RentalVacancyRate")],by=c("Series"="RentalVacancyRate")) %>%
  select(-c("Series","Date"))


# yearly
HomeownershipRate_df <- NULL
#idx <- 1
for (idx in seq(length(HomeownershipRate))){
  HomeownershipRate_index <- HomeownershipRate[idx]
  temp_df <- as.data.frame(getSymbols(HomeownershipRate_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = HomeownershipRate_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("HomeownershipRate", "Date", "Series")
  HomeownershipRate_df = rbind(HomeownershipRate_df, temp_df)
}

HomeownershipRate_df <- HomeownershipRate_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","HomeownershipRate")],by=c("Series"="HomeownershipRate")) %>%
  select(-c("Series","Date"))


#### quarterly
RealEstateRentalLeasingEarnings_df <- NULL
#idx <- 1
for (idx in seq(length(RealEstateRentalLeasingEarnings))){
  RealEstateRentalLeasingEarnings_index <- RealEstateRentalLeasingEarnings[idx]
  temp_df <- as.data.frame(getSymbols(RealEstateRentalLeasingEarnings_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = RealEstateRentalLeasingEarnings_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("RealEstateRentalLeasingEarnings", "Date", "Series")
  RealEstateRentalLeasingEarnings_df = rbind(RealEstateRentalLeasingEarnings_df, temp_df)
}
RealEstateRentalLeasingEarnings_df <- RealEstateRentalLeasingEarnings_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date),
         quarter=yearquarter(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","RealEstateRentalLeasingEarnings")],by=c("Series"="RealEstateRentalLeasingEarnings")) %>%
  select(-c("Series"))


########

# monthly
UnemploymentRate_df <- NULL
#idx <- 1
for (idx in seq(length(UnemploymentRate))){
  UnemploymentRate_index <- UnemploymentRate[idx]
  temp_df <- as.data.frame(getSymbols(UnemploymentRate_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = UnemploymentRate_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("UnemploymentRate", "Date", "Series")
  UnemploymentRate_df = rbind(UnemploymentRate_df, temp_df)
}
UnemploymentRate_df <- UnemploymentRate_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date),
         quarter=yearquarter(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","UnemploymentRate")],by=c("Series"="UnemploymentRate")) %>%
  select(-c("Series")) %>%
  group_by(State, quarter) %>%
  mutate(UnemploymentRate = mean(UnemploymentRate)) %>%
  distinct(State, quarter, .keep_all=TRUE)


# quarterly
GDPTotal_df <- NULL
#idx <- 1
for (idx in seq(length(GDPTotal))){
  GDPTotal_index <- GDPTotal[idx]
  temp_df <- as.data.frame(getSymbols(GDPTotal_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = GDPTotal_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("GDPTotal", "Date", "Series")
  GDPTotal_df = rbind(GDPTotal_df, temp_df)
}
GDPTotal_df <- GDPTotal_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date),
         quarter=yearquarter(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","GDPTotal")],by=c("Series"="GDPTotal")) %>%
  select(-c("Series"))



# monthly
BuildingPermits_df <- NULL
#idx <- 1
for (idx in seq(length(BuildingPermits))){
  BuildingPermits_index <- BuildingPermits[idx]
  temp_df <- as.data.frame(getSymbols(BuildingPermits_index, src = "FRED",auto.assign = FALSE))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = BuildingPermits_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("BuildingPermits", "Date", "Series")
  BuildingPermits_df = rbind(BuildingPermits_df, temp_df)
}
BuildingPermits_df <- BuildingPermits_df %>%
  mutate(Date = as.Date(Date),
         year = year(Date),
         quarter=yearquarter(Date)) %>%
  filter(Date >=histStartDt & Date <= histEndDt) %>%
  left_join(indices_raw[,c("State","StateShortCut","BuildingPermits")],by=c("Series"="BuildingPermits")) %>%
  select(-c("Series")) %>%
  group_by(State, quarter) %>%
  mutate(BuildingPermits = mean(BuildingPermits)) %>%
  distinct(State, quarter, .keep_all=TRUE)



################################################################

### Take all regional data and merge them
Regional_Data <- BuildingPermits_df %>%
  left_join(GDPTotal_df, by=c("State","StateShortCut","quarter","year","Date")) %>%
  left_join(UnemploymentRate_df, by=c("State","StateShortCut","quarter","year","Date")) %>%
  left_join(RealEstateRentalLeasingEarnings_df, by=c("State","StateShortCut","quarter","year","Date")) %>%
  left_join(AllTransHousePriceIndex_df, by=c("State","StateShortCut","quarter","year","Date")) %>%
  left_join(HomeownershipRate_df, by=c("State","StateShortCut","year")) %>%
  left_join(RentalVacancyRate_df, by=c("State","StateShortCut","year")) %>%
  left_join(HomeVacancyRate_df, by=c("State","StateShortCut","year")) %>%
  filter(Date != "2021-01-01") %>%
  group_by(State) %>%
  mutate(GDPTotalGrowthqtly = 100*(GDPTotal/lag(GDPTotal,n=1L)-1)) %>%
  mutate(GDPTotalGrowthyrly = 100*(GDPTotal/lag(GDPTotal,n=4L)-1)) %>%
  mutate(RealEstateRentalLeasingEarningsGrowth = 100*(RealEstateRentalLeasingEarnings/lag(RealEstateRentalLeasingEarnings,n=1L)-1)) %>%
  mutate(HousePriceIndexGrowthqtly = 100*(HousePriceIndex/lag(HousePriceIndex,n=1L)-1)) %>%
  mutate(HousePriceIndexGrowthyrly = 100*(HousePriceIndex/lag(HousePriceIndex,n=4L)-1)) %>%
  mutate(HousePriceIndexGrowthLongSpell = 100*(HousePriceIndex/lag(HousePriceIndex,n=12L)-1)) %>%
  mutate(UnemploymentRateLag = lag(UnemploymentRate,n=1L)) %>%
  mutate(HousePriceIndexGrowthqtlyLag = lag(HousePriceIndexGrowthqtly,n=1L)) %>%
  mutate(HousePriceIndexGrowthyrlyLag = lag(HousePriceIndexGrowthyrly,n=1L)) %>%
  mutate(HousePriceIndexGrowthLongSpellLag = lag(HousePriceIndexGrowthLongSpell,n=1L)) %>%
  mutate(GDPTotalGrowthqtlyLag = lag(GDPTotalGrowthqtly,n=1L)) %>%
  mutate(GDPTotalGrowthyrlyLag = lag(GDPTotalGrowthyrly,n=1L)) %>%
  mutate(HomeownershipRateLag = lag(HomeownershipRate,n=1L)) %>%
  mutate(RentalVacancyRateLag = lag(RentalVacancyRate,n=1L)) %>%
  mutate(HomeVacancyRateLag = lag(HomeVacancyRate,n=1L))


Regional_Data <-Regional_Data %>%
  mutate(UNRATE_Reg_OneQrterLag = lag(UnemploymentRate, n = 1L),
         quarter = yearquarter(Date),
         HomeOwnerShipRate_Reg_OneYearLag = lag(HomeownershipRate, n = 4L),
         HomeVacancyRate_Reg_OneYearLag = lag(HomeVacancyRate, n = 4L),
         RentalVacancyRate_Reg_OneYearLag = lag(RentalVacancyRate, n = 4L),
         HousePriceIndex_QoQ_OneQrterLag = lag(100*(HousePriceIndex/lag(HousePriceIndex,n=1L)-1), n = 1L)) %>%
  select("UNRATE_Reg_OneQrterLag","quarter","StateShortCut","HomeOwnerShipRate_Reg_OneYearLag","HomeVacancyRate_Reg_OneYearLag","RentalVacancyRate_Reg_OneYearLag","HousePriceIndex_QoQ_OneQrterLag")



################################################################
############ NATIONAL INDICATORS ###############################
################################################################


## downloadable from https://research.stlouisfed.org/econ/mccracken/fred-databases/
US_national_data <- read_excel("~/MortgageTrend/US_macro_data.xlsx", sheet ="current")

US_national_data <- US_national_data %>%
  mutate(term_spread_long_OneMLag = lag((GS10-GS1), n = 1L),
         term_spread_short_OneMLag= lag((GS1-TB3MS), n = 1L),
         paper_spread_OneMLag = lag(CP3Mx-TB3MS, n = 1L),
         HOUSTNE_MoM_OneMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=1L)-1), n = 1L),
         HOUSTNE_MoM_OneMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=1)-1),n=1),
         HOUSTMW_MoM_OneMLag = lag(100*(HOUSTMW/lag(HOUSTMW,n=1)-1),n=1),
         HOUSTS_MoM_OneMLag = lag(100*(HOUSTS/lag(HOUSTS,n=1)-1),n=1),
         HOUSTW_MoM_OneMLag = lag(100*(HOUSTW/lag(HOUSTW,n=1)-1),n=1),
         PERMITNE_MoM_OneMLag = lag(100*(PERMITNE/lag(PERMITNE,n=1)-1),n=1),
         PERMITMW_MoM_OneMLag = lag(100*(PERMITMW/lag(PERMITMW,n=1)-1),n=1),
         PERMITS_MoM_OneMLag = lag(100*(PERMITS/lag(PERMITS,n=1)-1),n=1),
         PERMITW_MoM_OneMLag = lag(100*(PERMITW/lag(PERMITW,n=1)-1),n=1),
         HOUSTNE_YoY_OneMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=12)-1),n=1),
         HOUSTMW_YoY_OneMLag = lag(100*(HOUSTMW/lag(HOUSTMW,n=12)-1),n=1),
         HOUSTS_YoY_OneMLag = lag(100*(HOUSTS/lag(HOUSTS,n=12)-1),n=1),
         HOUSTW_YoY_OneMLag = lag(100*(HOUSTW/lag(HOUSTW,n=12)-1),n=1),
         PERMITNE_YoY_OneMLag = lag(100*(PERMITNE/lag(PERMITNE,n=12)-1),n=1),
         PERMITMW_YoY_OneMLag = lag(100*(PERMITMW/lag(PERMITMW,n=12)-1),n=1),
         PERMITS_YoY_OneMLag = lag(100*(PERMITS/lag(PERMITS,n=12)-1),n=1),
         PERMITW_YoY_OneMLag = lag(100*(PERMITW/lag(PERMITW,n=12)-1),n=1),
         spread_OneMLag = lag(BAA, n = 1),
         paper_spread_OneMLag = lag(CP3Mx-TB3MS, n = 1),
         Date = as.Date(sasdate),
         VIX_OneMLag = lag(VXOCLSx, n = 1),
         OILPRICEx_MoM_OneMLag = lag(100*(OILPRICEx/lag(OILPRICEx,n=1)-1),n=1),
         OILPRICEx_YoY_OneMLag = lag(100*(OILPRICEx/lag(OILPRICEx,n=12)-1),n=1),
         BUSLOANS_MoM_OneMLag = lag(100*(BUSLOANS/lag(BUSLOANS,n=1)-1),n=1),
         BUSLOANS_YoY_OneMLag = lag(100*(BUSLOANS/lag(BUSLOANS,n=12)-1),n=1),
         CONSPI_MoM_OneMLag = lag(100*(CONSPI/lag(CONSPI,n=1)-1),n=1),
         CONSPI_YoY_OneMLag = lag(100*(CONSPI/lag(CONSPI,n=12)-1),n=1),
         RealEstate_MoM_OneMLag = lag(100*(REALLN/lag(REALLN,n=1)-1),n=1),
         RealEstate_YoY_OneMLag = lag(100*(REALLN/lag(REALLN,n=12)-1),n=1),
         SP_500_MoM_OneMLag = lag(100*(SP_500/lag(SP_500,n=1)-1),n=1),
         SP_500_YoY_OneMLag = lag(100*(SP_500/lag(SP_500,n=12)-1),n=1),
         INDPRO_MoM_OneMLag = lag(100*(INDPRO/lag(INDPRO,n=1)-1),n=1),
         INDPRO_YoY_OneMLag = lag(100*(INDPRO/lag(INDPRO,n=12)-1),n=1),
         IPFINAL_MoM_OneMLag = lag(100*(IPFINAL/lag(IPFINAL,n=1)-1),n=1),
         IPFINAL_YoY_OneMLag = lag(100*(IPFINAL/lag(IPFINAL,n=12)-1),n=1),
         term_spread_long_TwoMLag = lag((GS10-GS1), n = 2L),
         term_spread_short_TwoMLag= lag((GS1-TB3MS), n = 2L),
         paper_spread_TwoMLag = lag(CP3Mx-TB3MS, n = 2L),
         HOUSTNE_MoM_TwoMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=1L)-1), n = 2L),
         HOUSTNE_MoM_TwoMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=1)-1),n=2),
         HOUSTMW_MoM_TwoMLag = lag(100*(HOUSTMW/lag(HOUSTMW,n=1)-1),n=2),
         HOUSTS_MoM_TwoMLag = lag(100*(HOUSTS/lag(HOUSTS,n=1)-1),n=2),
         HOUSTW_MoM_TwoMLag = lag(100*(HOUSTW/lag(HOUSTW,n=1)-1),n=2),
         PERMITNE_MoM_TwoMLag = lag(100*(PERMITNE/lag(PERMITNE,n=1)-1),n=2),
         PERMITMW_MoM_TwoMLag = lag(100*(PERMITMW/lag(PERMITMW,n=1)-1),n=2),
         PERMITS_MoM_TwoMLag = lag(100*(PERMITS/lag(PERMITS,n=1)-1),n=2),
         PERMITW_MoM_TwoMLag = lag(100*(PERMITW/lag(PERMITW,n=1)-1),n=2),
         HOUSTNE_YoY_TwoMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=12)-1),n=2),
         HOUSTMW_YoY_TwoMLag = lag(100*(HOUSTMW/lag(HOUSTMW,n=12)-1),n=2),
         HOUSTS_YoY_TwoMLag = lag(100*(HOUSTS/lag(HOUSTS,n=12)-1),n=2),
         HOUSTW_YoY_TwoMLag = lag(100*(HOUSTW/lag(HOUSTW,n=12)-1),n=2),
         PERMITNE_YoY_TwoMLag = lag(100*(PERMITNE/lag(PERMITNE,n=12)-1),n=2),
         PERMITMW_YoY_TwoMLag = lag(100*(PERMITMW/lag(PERMITMW,n=12)-1),n=2),
         PERMITS_YoY_TwoMLag = lag(100*(PERMITS/lag(PERMITS,n=12)-1),n=2),
         PERMITW_YoY_TwoMLag = lag(100*(PERMITW/lag(PERMITW,n=12)-1),n=2),
         spread_TwoMLag = lag(BAA, n = 2),
         paper_spread_TwoMLag = lag(CP3Mx-TB3MS, n = 2),
         Date = as.Date(sasdate),
         VIX_TwoMLag = lag(VXOCLSx, n = 2),
         OILPRICEx_MoM_TwoMLag = lag(100*(OILPRICEx/lag(OILPRICEx,n=1)-1),n=2),
         OILPRICEx_YoY_TwoMLag = lag(100*(OILPRICEx/lag(OILPRICEx,n=12)-1),n=2),
         BUSLOANS_MoM_TwoMLag = lag(100*(BUSLOANS/lag(BUSLOANS,n=1)-1),n=2),
         BUSLOANS_YoY_TwoMLag = lag(100*(BUSLOANS/lag(BUSLOANS,n=12)-1),n=2),
         CONSPI_MoM_TwoMLag = lag(100*(CONSPI/lag(CONSPI,n=1)-1),n=2),
         CONSPI_YoY_TwoMLag = lag(100*(CONSPI/lag(CONSPI,n=12)-1),n=2),
         RealEstate_MoM_TwoMLag = lag(100*(REALLN/lag(REALLN,n=1)-1),n=2),
         RealEstate_YoY_TwoMLag = lag(100*(REALLN/lag(REALLN,n=12)-1),n=2),
         SP_500_MoM_TwoMLag = lag(100*(SP_500/lag(SP_500,n=1)-1),n=2),
         SP_500_YoY_TwoMLag = lag(100*(SP_500/lag(SP_500,n=12)-1),n=2),
         INDPRO_MoM_TwoMLag = lag(100*(INDPRO/lag(INDPRO,n=1)-1),n=2),
         INDPRO_YoY_TwoMLag = lag(100*(INDPRO/lag(INDPRO,n=12)-1),n=2),
         IPFINAL_MoM_TwoMLag = lag(100*(IPFINAL/lag(IPFINAL,n=1)-1),n=2),
         IPFINAL_YoY_TwoMLag = lag(100*(IPFINAL/lag(IPFINAL,n=12)-1),n=2),
         term_spread_long_ThreeMLag = lag((GS10-GS1), n = 3L),
         term_spread_short_ThreeMLag = lag((GS1-TB3MS), n = 3L),
         paper_spread_ThreeMLag = lag(CP3Mx-TB3MS, n = 3L),
         HOUSTNE_MoM_ThreeMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=1L)-1), n = 3L),
         HOUSTNE_MoM_ThreeMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=1)-1),n=3),
         HOUSTMW_MoM_ThreeMLag = lag(100*(HOUSTMW/lag(HOUSTMW,n=1)-1),n=3),
         HOUSTS_MoM_ThreeMLag = lag(100*(HOUSTS/lag(HOUSTS,n=1)-1),n=3),
         HOUSTW_MoM_ThreeMLag = lag(100*(HOUSTW/lag(HOUSTW,n=1)-1),n=3),
         PERMITNE_MoM_ThreeMLag = lag(100*(PERMITNE/lag(PERMITNE,n=1)-1),n=3),
         PERMITMW_MoM_ThreeMLag = lag(100*(PERMITMW/lag(PERMITMW,n=1)-1),n=3),
         PERMITS_MoM_ThreeMLag = lag(100*(PERMITS/lag(PERMITS,n=1)-1),n=3),
         PERMITW_MoM_ThreeMLag = lag(100*(PERMITW/lag(PERMITW,n=1)-1),n=3),
         HOUSTNE_YoY_ThreeMLag = lag(100*(HOUSTNE/lag(HOUSTNE,n=12)-1),n=3),
         HOUSTMW_YoY_ThreeMLag = lag(100*(HOUSTMW/lag(HOUSTMW,n=12)-1),n=3),
         HOUSTS_YoY_ThreeMLag = lag(100*(HOUSTS/lag(HOUSTS,n=12)-1),n=3),
         HOUSTW_YoY_ThreeMLag = lag(100*(HOUSTW/lag(HOUSTW,n=12)-1),n=3),
         PERMITNE_YoY_ThreeMLag = lag(100*(PERMITNE/lag(PERMITNE,n=12)-1),n=3),
         PERMITMW_YoY_ThreeMLag = lag(100*(PERMITMW/lag(PERMITMW,n=12)-1),n=3),
         PERMITS_YoY_ThreeMLag = lag(100*(PERMITS/lag(PERMITS,n=12)-1),n=3),
         PERMITW_YoY_ThreeMLag = lag(100*(PERMITW/lag(PERMITW,n=12)-1),n=3),
         spread_ThreeMLag = lag(BAA, n = 3),
         paper_spread_ThreeMLag = lag(CP3Mx-TB3MS, n = 3),
         Date = as.Date(sasdate),
         VIX_ThreeMLag = lag(VXOCLSx, n = 3),
         OILPRICEx_MoM_ThreeMLag = lag(100*(OILPRICEx/lag(OILPRICEx,n=1)-1),n=3),
         OILPRICEx_YoY_ThreeMLag = lag(100*(OILPRICEx/lag(OILPRICEx,n=12)-1),n=3),
         BUSLOANS_MoM_ThreeMLag = lag(100*(BUSLOANS/lag(BUSLOANS,n=1)-1),n=3),
         BUSLOANS_YoY_ThreeMLag = lag(100*(BUSLOANS/lag(BUSLOANS,n=12)-1),n=3),
         CONSPI_MoM_ThreeMLag = lag(100*(CONSPI/lag(CONSPI,n=1)-1),n=3),
         CONSPI_YoY_ThreeMLag = lag(100*(CONSPI/lag(CONSPI,n=12)-1),n=3),
         RealEstate_MoM_ThreeMLag = lag(100*(REALLN/lag(REALLN,n=1)-1),n=3),
         RealEstate_YoY_ThreeMLag = lag(100*(REALLN/lag(REALLN,n=12)-1),n=3),
         SP_500_MoM_ThreeMLag = lag(100*(SP_500/lag(SP_500,n=1)-1),n=3),
         SP_500_YoY_ThreeMLag = lag(100*(SP_500/lag(SP_500,n=12)-1),n=3),
         INDPRO_MoM_ThreeMLag = lag(100*(INDPRO/lag(INDPRO,n=1)-1),n=3),
         INDPRO_YoY_ThreeMLag = lag(100*(INDPRO/lag(INDPRO,n=12)-1),n=3),
         IPFINAL_MoM_ThreeMLag = lag(100*(IPFINAL/lag(IPFINAL,n=1)-1),n=3),
         IPFINAL_YoY_ThreeMLag = lag(100*(IPFINAL/lag(IPFINAL,n=12)-1),n=3),
         UNRATE_ThreeMLag = lag(UNRATE, n = 1),
         UNRATE_TwoMLag = lag(UNRATE, n = 2),
         UNRATE_OneMLag = lag(UNRATE, n = 3)) %>% select(c("Date",
                                                           "term_spread_long_ThreeMLag","term_spread_short_ThreeMLag","paper_spread_ThreeMLag",
                                                           "HOUSTNE_MoM_ThreeMLag","HOUSTMW_MoM_ThreeMLag","HOUSTS_MoM_ThreeMLag","HOUSTW_MoM_ThreeMLag","PERMITNE_MoM_ThreeMLag",
                                                           "PERMITNE_MoM_ThreeMLag","PERMITMW_MoM_ThreeMLag","PERMITS_MoM_ThreeMLag","PERMITW_MoM_ThreeMLag",
                                                           "HOUSTS_YoY_ThreeMLag","HOUSTNE_YoY_ThreeMLag","HOUSTW_YoY_ThreeMLag","PERMITNE_YoY_ThreeMLag","PERMITMW_YoY_ThreeMLag",
                                                           "PERMITS_YoY_ThreeMLag","PERMITW_YoY_ThreeMLag","spread_ThreeMLag","paper_spread_ThreeMLag","VIX_ThreeMLag","OILPRICEx_MoM_ThreeMLag",
                                                           "OILPRICEx_YoY_ThreeMLag","BUSLOANS_MoM_ThreeMLag","BUSLOANS_YoY_ThreeMLag",
                                                           "CONSPI_MoM_ThreeMLag","CONSPI_YoY_ThreeMLag","RealEstate_MoM_ThreeMLag","RealEstate_YoY_ThreeMLag",
                                                           "SP_500_MoM_ThreeMLag","SP_500_YoY_ThreeMLag","INDPRO_MoM_ThreeMLag","INDPRO_YoY_ThreeMLag",
                                                           "IPFINAL_MoM_ThreeMLag","IPFINAL_YoY_ThreeMLag","UNRATE_ThreeMLag",
                                                           "term_spread_long_TwoMLag","term_spread_short_TwoMLag","paper_spread_TwoMLag",
                                                           "HOUSTNE_MoM_TwoMLag","HOUSTMW_MoM_TwoMLag","HOUSTS_MoM_TwoMLag","HOUSTW_MoM_TwoMLag","PERMITNE_MoM_TwoMLag",
                                                           "PERMITNE_MoM_TwoMLag","PERMITMW_MoM_TwoMLag","PERMITS_MoM_TwoMLag","PERMITW_MoM_TwoMLag",
                                                           "HOUSTS_YoY_TwoMLag","HOUSTNE_YoY_TwoMLag","HOUSTW_YoY_TwoMLag","PERMITNE_YoY_TwoMLag","PERMITMW_YoY_TwoMLag",
                                                           "PERMITS_YoY_TwoMLag","PERMITW_YoY_TwoMLag","spread_TwoMLag","paper_spread_TwoMLag","VIX_TwoMLag","OILPRICEx_MoM_TwoMLag",
                                                           "OILPRICEx_YoY_TwoMLag","BUSLOANS_MoM_TwoMLag","BUSLOANS_YoY_TwoMLag",
                                                           "CONSPI_MoM_TwoMLag","CONSPI_YoY_TwoMLag","RealEstate_MoM_TwoMLag","RealEstate_YoY_TwoMLag",
                                                           "SP_500_MoM_TwoMLag","SP_500_YoY_TwoMLag","INDPRO_MoM_TwoMLag","INDPRO_YoY_TwoMLag",
                                                           "IPFINAL_MoM_TwoMLag","IPFINAL_YoY_TwoMLag","UNRATE_TwoMLag",
                                                           "term_spread_long_OneMLag","term_spread_short_OneMLag","paper_spread_OneMLag",
                                                           "HOUSTNE_MoM_OneMLag","HOUSTMW_MoM_OneMLag","HOUSTS_MoM_OneMLag","HOUSTW_MoM_OneMLag","PERMITNE_MoM_OneMLag",
                                                           "PERMITNE_MoM_OneMLag","PERMITMW_MoM_OneMLag","PERMITS_MoM_OneMLag","PERMITW_MoM_OneMLag",
                                                           "HOUSTS_YoY_OneMLag","HOUSTNE_YoY_OneMLag","HOUSTW_YoY_OneMLag","PERMITNE_YoY_OneMLag","PERMITMW_YoY_OneMLag",
                                                           "PERMITS_YoY_OneMLag","PERMITW_YoY_OneMLag","spread_OneMLag","paper_spread_OneMLag","VIX_OneMLag","OILPRICEx_MoM_OneMLag",
                                                           "OILPRICEx_YoY_OneMLag","BUSLOANS_MoM_OneMLag","BUSLOANS_YoY_OneMLag",
                                                           "CONSPI_MoM_OneMLag","CONSPI_YoY_OneMLag","RealEstate_MoM_OneMLag","RealEstate_YoY_OneMLag",
                                                           "SP_500_MoM_OneMLag","SP_500_YoY_OneMLag","INDPRO_MoM_OneMLag","INDPRO_YoY_OneMLag",
                                                           "IPFINAL_MoM_OneMLag","IPFINAL_YoY_OneMLag","UNRATE_OneMLag")) %>% mutate(month = yearmonth(Date))




write.csv(BuildingPermits_df,"~/MortgageTrend/BuildingPermits_df.csv", row.names = FALSE)
write.csv(AllTransHousePriceIndex_df,"~/MortgageTrend/AllTransHousePriceIndex_df.csv", row.names = FALSE)
write.csv(GDPTotal_df,"~/MortgageTrend/GDPTotal_df.csv", row.names = FALSE)
write.csv(HomeownershipRate_df,"~/MortgageTrend/HomeownershipRate_df.csv", row.names = FALSE)
write.csv(HomeVacancyRate_df,"~/MortgageTrend/HomeVacancyRate_df.csv", row.names = FALSE)
write.csv(RealEstateRentalLeasingEarnings_df,"~/MortgageTrend/RealEstateRentalLeasingEarnings_df.csv", row.names = FALSE)
write.csv(RentalVacancyRate_df,"~/MortgageTrend/RentalVacancyRate_df.csv", row.names = FALSE)
write.csv(UnemploymentRate_df,"~/MortgageTrend/UnemploymentRate_df.csv", row.names = FALSE)


write.csv(Regional_Data,"~/MortgageTrend/Regional_Data.csv", row.names = FALSE)

