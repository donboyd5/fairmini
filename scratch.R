
# run setup first

fmall <- readRDS(fs::path(DDATA, "fmall.rds"))
# glimpse(fmall)
ns(fmall)

# Peak-to-peak interpolation of CD/POP. Peaks are
# 1953:1, 1955:3, 1960:2, 1963:2, 1965:4, 1968:3,
# 1973:2, 1978:4, 1985:1, 1988:4, 1994:1, 1995:4,
# 2000:3, 2007:2, 2012:1, 2013:4.

# note that Fair's data begins before the sample start so calculate CDA before filtering
peaksq <- c("1953q1", "1955q3", "1960q2", "1963q2", "1965q4", "1968q3", "1973q2", "1978q4", "1985q1", "1988q4", "1994q1", "1995q4", "2000q3", "2007q2", "2012q1", "2013q4") # 16
peaksd <- lubridate::yq(peaksq)

df <- fmall |> 
  select(date, CD_pop) |> 
  mutate(row=row_number())

library(pracma)
(peaks <- findpeaks(df$CD_pop))
# height, then indexes of max, begin, end
# [,1] [,2] [,3] [,4]
# [1,] 0.1601615    7    1   11 1955-07-01 yes
# [2,] 0.1517271   13   11   18 1957-01-01
# [3,] 0.1528516   23   18   24 1959-07-01
# [4,] 0.1518231   26   24   29 1960-04-01 yes
# [5,] 0.1856577   43   29   44
# [6,] 0.1951881   45   44   46
# [7,] 0.2174725   49   46   50
# [8,] 0.2139689   51   50   53
# [9,] 0.2163176   54   53   56
# [10,] 0.2386629   59   56   60


df[18:24,]

df |> 
  ggplot(aes(date, CD_pop)) +
  geom_line(colour="blue", linewidth=1) +
  geom_vline(xintercept = peaksd) +
  scale_x_date(name=NULL, date_breaks = "5 years", date_labels = "%Y-%b") +
  theme_bw() +
  x90 +
  ggtitle("Ray Fair's peak time points for CD/POP with current (revised) CD/POP data")

df

df2 <- df |> 
  mutate(peak = date %in% peaksd,
         peak = ifelse(row_number()==1 | row_number()==max(row_number()), TRUE, peak),
         CDA = ifelse(peak, CD_pop, NA_real_),
         CDA2 = CDA,
         approx(date, CDA2, xout = date, method = "linear", rule = 2)$y)

df2 <- df |> 
  mutate(peak = date %in% peaksd,
         peak = ifelse(row_number()==1 | row_number()==max(row_number()), TRUE, peak),
         CDA = ifelse(peak, CD_pop, NA_real_),
         CDA = stats::approx(date, CDA, xout = date, method = "linear", rule = 2)$y)
  


