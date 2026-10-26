Analyses for Kowalewski et al.
================
Griffin Kowalewski
2026-10-22

## R Markdown

\#Plotting for Figure 1C

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(stringr)
library(extrafont)
```

    ## Registering fonts with R

``` r
library(ggbreak)
```

    ## ggbreak v0.2.2 Learn more at https://yulab-smu.top/

    ## If you use ggbreak in published research, please cite the following
    ## paper:
    ## 
    ## S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. Use ggbreak to effectively
    ## utilize plotting space to deal with large datasets and outliers.
    ## Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846

``` r
# Load and preprocess
 all_data = as.data.frame(read.csv(file="Data/Fig1C.csv", header = T))
 all_data$experiment <- factor(all_data$experiment)
 
# Add treatment formatting
all_data <- all_data %>%
  mutate(
    treatment = factor(treatment),
    treatment_wrapped = str_wrap(as.character(treatment), width = 8),
    treatment_num = as.numeric(treatment)
  )

# Calculate bar means and SE manually
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se_val = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
set.seed(123)
# Plot
ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = 0.65, size = 0.2) +

  geom_errorbar(data = bar_means,
                aes(x = treatment_num, ymin = mean_val - se_val, ymax = mean_val + se_val),
                width = 0.2, size = 0.2, color = "black") +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2),
             size = 1.0, alpha = 0.8, stroke = NA) +

  scale_x_continuous(
    breaks = sort(unique(all_data$treatment_num)),
    labels = bar_means$treatment_wrapped,
    expand = expansion(mult = c(0.06, .35))
  ) +

  scale_y_continuous(limits = c(-.01, 5), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(values = c("1" = "#0A9F9D", "2" = "#E54E21", "3" = "#6C8645")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 6),  # Fallback if Arial not installed
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.line = element_line(color = 'black', size = 0.3),
    axis.ticks = element_line(size = 0.3),
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("Total number of cells per second") +
  coord_cartesian(clip = "off", xlim = c(0.8, 6.4))
```

    ## Warning in geom_col(data = bar_means, aes(x = treatment_num, y = mean_val), :
    ## Ignoring unknown parameters: `size`

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#Save the plot
ggsave("Fig1C.pdf",
       path = "Figures",
       width = 4, height = 5, units = "in", dpi = 300)
```

\#Stats for Figure 1C

``` r
anova(aov(all_data$value ~ all_data$treatment_wrapped))
```

    ## Analysis of Variance Table
    ## 
    ## Response: all_data$value
    ##                            Df Sum Sq Mean Sq F value    Pr(>F)    
    ## all_data$treatment_wrapped  5 16.305  3.2611  11.407 2.763e-07 ***
    ## Residuals                  48 13.722  0.2859                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(aov(all_data$value ~ all_data$treatment_wrapped))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = all_data$value ~ all_data$treatment_wrapped)
    ## 
    ## $`all_data$treatment_wrapped`
    ##                                                         diff        lwr
    ## Ambient\nHost\ncells-Ambient\nFree\nalgae        -0.01851852 -0.7665778
    ## Ambient\nSymbiotic\nalgae-Ambient\nFree\nalgae    0.04444444 -0.7036148
    ## Heat\nFree\nalgae-Ambient\nFree\nalgae            0.11728395 -0.6307753
    ## Heat\nHost\ncells-Ambient\nFree\nalgae            0.24567901 -0.5023803
    ## Heat\nSymbiotic\nalgae-Ambient\nFree\nalgae       1.53333333  0.7852740
    ## Ambient\nSymbiotic\nalgae-Ambient\nHost\ncells    0.06296296 -0.6850963
    ## Heat\nFree\nalgae-Ambient\nHost\ncells            0.13580247 -0.6122568
    ## Heat\nHost\ncells-Ambient\nHost\ncells            0.26419753 -0.4838618
    ## Heat\nSymbiotic\nalgae-Ambient\nHost\ncells       1.55185185  0.8037926
    ## Heat\nFree\nalgae-Ambient\nSymbiotic\nalgae       0.07283951 -0.6752198
    ## Heat\nHost\ncells-Ambient\nSymbiotic\nalgae       0.20123457 -0.5468247
    ## Heat\nSymbiotic\nalgae-Ambient\nSymbiotic\nalgae  1.48888889  0.7408296
    ## Heat\nHost\ncells-Heat\nFree\nalgae               0.12839506 -0.6196642
    ## Heat\nSymbiotic\nalgae-Heat\nFree\nalgae          1.41604938  0.6679901
    ## Heat\nSymbiotic\nalgae-Heat\nHost\ncells          1.28765432  0.5395950
    ##                                                        upr     p adj
    ## Ambient\nHost\ncells-Ambient\nFree\nalgae        0.7295408 0.9999997
    ## Ambient\nSymbiotic\nalgae-Ambient\nFree\nalgae   0.7925037 0.9999747
    ## Heat\nFree\nalgae-Ambient\nFree\nalgae           0.8653432 0.9971098
    ## Heat\nHost\ncells-Ambient\nFree\nalgae           0.9937383 0.9237189
    ## Heat\nSymbiotic\nalgae-Ambient\nFree\nalgae      2.2813926 0.0000027
    ## Ambient\nSymbiotic\nalgae-Ambient\nHost\ncells   0.8110222 0.9998585
    ## Heat\nFree\nalgae-Ambient\nHost\ncells           0.8838618 0.9942439
    ## Heat\nHost\ncells-Ambient\nHost\ncells           1.0122568 0.8989468
    ## Heat\nSymbiotic\nalgae-Ambient\nHost\ncells      2.2999111 0.0000021
    ## Heat\nFree\nalgae-Ambient\nSymbiotic\nalgae      0.8208988 0.9997106
    ## Heat\nHost\ncells-Ambient\nSymbiotic\nalgae      0.9492939 0.9664336
    ## Heat\nSymbiotic\nalgae-Ambient\nSymbiotic\nalgae 2.2369482 0.0000050
    ## Heat\nHost\ncells-Heat\nFree\nalgae              0.8764543 0.9955710
    ## Heat\nSymbiotic\nalgae-Heat\nFree\nalgae         2.1641087 0.0000137
    ## Heat\nSymbiotic\nalgae-Heat\nHost\ncells         2.0357136 0.0000783

\#Plotting for Figure 1F and 1G

``` r
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# Load data
all_data <- read.csv(file = "Data/Fig1F.csv", header = TRUE)

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

all_data <- all_data %>%
  mutate(treatment = str_trim(as.character(treatment))) %>%       # tidy labels
  filter(!is.na(treatment), treatment != "", !is.na(value)) %>%   # drop blank rows
  filter(!str_detect(treatment, regex("host", ignore_case = TRUE))) %>%  # REMOVE host cells
  mutate(
    treatment         = droplevels(factor(treatment)),
    treatment_num     = as.integer(treatment),
    treatment_wrapped = str_wrap(as.character(treatment), width = 8)
  )

# Recalculate bar means (ordered to match the x positions)
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(treatment_num)

# Plot
p <- ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = 0.9, linewidth = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4,
             alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               linewidth = 0.2, color = "black") +

  scale_x_continuous(
    breaks = bar_means$treatment_num,
    labels = bar_means$treatment_wrapped,
    expand = expansion(add = 0.6)   # even padding on both sides
  ) +

  scale_y_continuous(
    limits = c(0, 1.0),
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) x * 100
  ) +

  scale_color_manual(values = c("1" = "#0A9F9D", "2" = "#E54E21")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x  = element_text(size = 6, color = "black"),
    axis.text.y  = element_text(size = 6, color = "black"),
    axis.line    = element_line(color = "black", linewidth = 0.3),
    axis.ticks   = element_line(linewidth = 0.3),
    plot.margin  = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("Percent of algae (%)")

p
```

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggsave("Fig1F.pdf", plot = p, path = "Figures",
       width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure 1F

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Symbiotic\nalgae') %>%
  pull(value)


b <- all_data %>%
  filter(treatment_wrapped == 'Free\nalgae') %>%
  pull(value)

t.test(a,b, var.equal=TRUE, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  a and b
    ## t = 3.9662, df = 5, p-value = 0.01068
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1125453 0.5271250
    ## sample estimates:
    ## mean difference 
    ##       0.3198351

\#Plot for Figure 1G

``` r
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# Load data
all_data <- read.csv(file = "Data/Fig1G.csv", header = TRUE)

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

all_data <- all_data %>%
  mutate(treatment = str_trim(as.character(treatment))) %>%       # tidy labels
  filter(!is.na(treatment), treatment != "", !is.na(value)) %>%   # drop blank rows
  filter(!str_detect(treatment, regex("host", ignore_case = TRUE))) %>%  # REMOVE host cells
  mutate(
    treatment         = droplevels(factor(treatment)),
    treatment_num     = as.integer(treatment),
    treatment_wrapped = str_wrap(as.character(treatment), width = 8)
  )

# Recalculate bar means (ordered to match the x positions)
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(treatment_num)

# Plot
p <- ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = 0.9, linewidth = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4,
             alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               linewidth = 0.2, color = "black") +

  scale_x_continuous(
    breaks = bar_means$treatment_num,
    labels = bar_means$treatment_wrapped,
    expand = expansion(add = 0.6)   # even padding on both sides
  ) +

  scale_y_continuous(
    limits = c(0, 1.0),
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) x * 100
  ) +

  scale_color_manual(values = c("1" = "#0A9F9D", "2" = "#E54E21")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x  = element_text(size = 6, color = "black"),
    axis.text.y  = element_text(size = 6, color = "black"),
    axis.line    = element_line(color = "black", linewidth = 0.3),
    axis.ticks   = element_line(linewidth = 0.3),
    plot.margin  = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("Percent of algae (%)")

p
```

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave("Fig1G.pdf", plot = p, path = "Figures",
       width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure 1G

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Symbiotic\nalgae') %>%
  pull(value)


b <- all_data %>%
  filter(treatment_wrapped == 'Free\nalgae') %>%
  pull(value)

t.test(a,b, var.equal=TRUE, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  a and b
    ## t = 6.1536, df = 5, p-value = 0.001648
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3077118 0.7492370
    ## sample estimates:
    ## mean difference 
    ##       0.5284744

\#Plotting for Figure 1J

``` r
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# Load data
all_data <- read.csv(file = "Data/Fig1J.csv", header = TRUE)

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

all_data <- all_data %>%
  mutate(treatment = str_trim(as.character(treatment))) %>%       # tidy labels
  filter(!is.na(treatment), treatment != "", !is.na(value)) %>%   # drop blank rows
  filter(!str_detect(treatment, regex("host", ignore_case = TRUE))) %>%  # REMOVE host cells
  mutate(
    treatment         = droplevels(factor(treatment)),
    treatment_num     = as.integer(treatment),
    treatment_wrapped = str_wrap(as.character(treatment), width = 8)
  )

# Recalculate bar means (ordered to match the x positions)
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(treatment_num)

# Plot
p <- ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = 0.9, linewidth = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4,
             alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               linewidth = 0.2, color = "black") +

  scale_x_continuous(
    breaks = bar_means$treatment_num,
    labels = bar_means$treatment_wrapped,
    expand = expansion(add = 0.6)   # even padding on both sides
  ) +

  scale_y_continuous(
    limits = c(0, 1.0),
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) x * 100
  ) +

  scale_color_manual(values = c("1" = "#0A9F9D", "2" = "#E54E21")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x  = element_text(size = 6, color = "black"),
    axis.text.y  = element_text(size = 6, color = "black"),
    axis.line    = element_line(color = "black", linewidth = 0.3),
    axis.ticks   = element_line(linewidth = 0.3),
    plot.margin  = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("Percent of algae (%)")

p
```

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("Fig1J.pdf", plot = p, path = "Figures",
       width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure 1J

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Symbiotic\nalgae') %>%
  pull(value)


b <- all_data %>%
  filter(treatment_wrapped == 'Free\nalgae') %>%
  pull(value)

t.test(a,b, var.equal=TRUE, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  a and b
    ## t = -0.56643, df = 5, p-value = 0.5956
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4478421  0.2861149
    ## sample estimates:
    ## mean difference 
    ##     -0.08086362

\#Plotting for Figure 2B

``` r
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# Load data
all_data <- read.csv(file = "Data/Fig2B.csv", header = TRUE)

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

# Clean and convert treatment to factor
all_data <- all_data %>%
  mutate(treatment = str_trim(as.character(treatment))) %>%
  filter(!is.na(treatment), treatment != "", !is.na(value)) %>%   # drop blank rows
  mutate(
    treatment         = droplevels(factor(treatment)),
    treatment_num     = as.integer(treatment),
    treatment_wrapped = str_wrap(as.character(treatment), width = 8)
  )

# Bar means (ordered to match x positions)
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(treatment_num)

n_bars <- nrow(bar_means)

# Plot
p <- ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = 0.9, linewidth = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4,
             alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               linewidth = 0.2, color = "black") +

  scale_x_continuous(
    breaks = bar_means$treatment_num,
    labels = bar_means$treatment_wrapped,
    limits = c(0.5, n_bars + 0.5),   # exactly the outer edges of the bars
    expand = c(0, 0)                 # no extra padding
  ) +

  scale_y_continuous(limits = c(-0.01, 10),
                     expand = expansion(mult = c(0, 0.05))) +

  scale_color_manual(values = c("1" = "black", "2" = "black")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x  = element_text(size = 6, color = "black"),
    axis.text.y  = element_text(size = 6, color = "black"),
    axis.line    = element_line(color = "black", linewidth = 0.3),
    axis.ticks   = element_line(linewidth = 0.3),
    plot.margin  = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab(expression("Mean area (" * mu * m^2 * ")")) +
  coord_cartesian(clip = "off")

p
```

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave("Fig2B.pdf", plot = p, path = "Figures",
       width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure 2B

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Dissociated') %>%
  pull(value)


b <- all_data %>%
  filter(treatment_wrapped == 'Expelled') %>%
  pull(value)

t.test(a,b, var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  a and b
    ## t = 9.5418, df = 10, p-value = 2.439e-06
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  4.111819 6.617173
    ## sample estimates:
    ## mean of x mean of y 
    ##  6.600263  1.235767

\#Plotting for Figure 2C

``` r
library(dplyr)
library(ggplot2)
library(stringr)  # Needed for str_wrap()
library(scales)

# Load data
all_data <- as.data.frame(read.csv(
  file = "Data/Fig2C.csv",
  header = TRUE
))

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

# Convert treatment to factor
all_data <- all_data %>%
  mutate(treatment = factor(treatment)) %>%
  mutate(
    treatment_wrapped = str_wrap(as.character(treatment), width = 8),
    treatment_num = as.numeric(treatment)
  )

# Recalculate bar_means after filtering
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

# Plot
ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = .9, size = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4, alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               size = 0.2, color = "black") +

  scale_x_continuous(
    breaks = sort(unique(all_data$treatment_num)),
    labels = bar_means$treatment_wrapped
  ) +

  scale_y_continuous(
  limits = c(0, 1.0),
  expand = expansion(mult = c(0, 0.05)),
  labels = function(x) x * 100  # Remove percent sign
) +

  scale_color_manual(values = c("1" = "#0A9F9D", "2" = "#E54E21")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.line = element_line(color = 'black', size = 0.3),
    axis.ticks = element_line(size = 0.3),
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("Percent of fragmented nuclei(%)")
```

    ## Warning in geom_col(data = bar_means, aes(x = treatment_num, y = mean_val), :
    ## Ignoring unknown parameters: `size`

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Save the plot
ggsave("Fig2C.pdf",
      path = "Figures",
      width = 2, height = 1.8, units = "in", dpi = 300)
```

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

\#Stats for Figure 2C

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Dissociated') %>%
  pull(value)


b <- all_data %>%
  filter(treatment_wrapped == 'Expelled') %>%
  pull(value)

t.test(a,b, var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  a and b
    ## t = -6.1822, df = 10, p-value = 0.0001038
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3993168 -0.1877351
    ## sample estimates:
    ##  mean of x  mean of y 
    ## 0.03258547 0.32611139

\#Plotting for Figure 2E

``` r
#Plotting for Figure 2E and S8C

library(dplyr)
library(ggplot2)
library(stringr)  # Needed for str_wrap()
library(scales)

# Load data
all_data <- as.data.frame(read.csv(
  file = "Data/Fig2E.csv",
  header = TRUE
))

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

# Convert treatment to factor
all_data <- all_data %>%
  mutate(treatment = factor(treatment)) %>% 
  mutate(
    treatment_wrapped = str_wrap(as.character(treatment), width = 8),
    treatment_num = as.numeric(treatment)
  )

# Recalculate bar_means after filtering
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

# Plot
ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = .9, size = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4, alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               size = 0.2, color = "black") +

  scale_x_continuous(
    breaks = sort(unique(all_data$treatment_num)),
    labels = bar_means$treatment_wrapped
  ) +

scale_y_continuous(limits = c(-.01, 12), expand = expansion(mult = c(0, 0.05))) +

  scale_color_manual(values = c("1" = "#009E73", "2" = "#D55E00", "3" = "#56B4E9")) +

  
  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.line = element_line(color = 'black', size = 0.3),
    axis.ticks = element_line(size = 0.3),
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("") 
```

    ## Warning in geom_col(data = bar_means, aes(x = treatment_num, y = mean_val), :
    ## Ignoring unknown parameters: `size`

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Save the plot
ggsave("Fig2E.pdf",
     path = "Figures",
     width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure 2E

``` r
#Stats for Figure 2E and S8C

library(rstatix)
```

    ## 
    ## Attaching package: 'rstatix'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Ambient_b01') %>%
  pull(value)

shapiro.test(a)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  a
    ## W = 0.61366, p-value = 9.273e-06

``` r
wilcox_test(value ~ treatment_wrapped, data = all_data, paired = FALSE)
```

    ## # A tibble: 1 × 7
    ##   .y.   group1      group2      n1    n2 statistic          p
    ## * <chr> <chr>       <chr>    <int> <int>     <dbl>      <dbl>
    ## 1 value Ambient_b01 Heat_b01    18    16        15 0.00000506

\#Plotting for Figure S8C

``` r
#Plotting for Figure 2E and S8C

library(dplyr)
library(ggplot2)
library(stringr)  # Needed for str_wrap()
library(scales)

# Load data
all_data <- as.data.frame(read.csv(
  file = "Data/FigS8C.csv",
  header = TRUE
))

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

# Convert treatment to factor
all_data <- all_data %>%
  mutate(treatment = factor(treatment)) %>% 
  mutate(
    treatment_wrapped = str_wrap(as.character(treatment), width = 8),
    treatment_num = as.numeric(treatment)
  )

# Recalculate bar_means after filtering
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

# Plot
ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = .9, size = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4, alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
               fun.data = mean_se, geom = "errorbar", width = 0.2,
               size = 0.2, color = "black") +

  scale_x_continuous(
    breaks = sort(unique(all_data$treatment_num)),
    labels = bar_means$treatment_wrapped
  ) +

scale_y_continuous(limits = c(-.01, 12), expand = expansion(mult = c(0, 0.05))) +

  scale_color_manual(values = c("1" = "#009E73", "2" = "#D55E00", "3" = "#56B4E9")) +

  
  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.line = element_line(color = 'black', size = 0.3),
    axis.ticks = element_line(size = 0.3),
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("") 
```

    ## Warning in geom_col(data = bar_means, aes(x = treatment_num, y = mean_val), :
    ## Ignoring unknown parameters: `size`

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Save the plot
ggsave("FigS8C.pdf",
     path = "Figures",
     width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure S8C

``` r
#Stats for Figure 2E and S8C

library(rstatix)

a <- all_data %>%
  filter(treatment_wrapped == 'Ambient_a01') %>%
  pull(value)

shapiro.test(a)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  a
    ## W = 0.56937, p-value = 5.337e-06

``` r
wilcox_test(value ~ treatment_wrapped, data = all_data, paired = FALSE)
```

    ## # A tibble: 1 × 7
    ##   .y.   group1      group2      n1    n2 statistic        p
    ## * <chr> <chr>       <chr>    <int> <int>     <dbl>    <dbl>
    ## 1 value Ambient_a01 Heat_a01    17    13        29 0.000291

\#Plotting for Figure 2G and S8D

``` r
library(dplyr)
library(ggplot2)
library(boot)

# Load data
all_data <- read.csv(
  "Data/Fig2G.csv",
  header = TRUE
)

# Ensure 'location' is a factor
all_data$location <- factor(all_data$location)

sum_in <- sum(all_data$value[all_data$location == "in"], na.rm = TRUE)
sum_in
```

    ## [1] 16.96816

``` r
sum_out <- sum(all_data$value[all_data$location == "out"], na.rm = TRUE)
sum_out
```

    ## [1] 36.00356

``` r
summary_data <- all_data %>%
  group_by(location) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(percent = total_value / sum(total_value) * 100)


ggplot(summary_data, aes(x = " ", y = percent, fill = location)) +
  geom_col(color = "black", width = 0.45, size = 0.25) +


  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_stack(vjust = 0.5),
    color = "black", size = 3
  ) +

  scale_y_continuous(
    labels = scales::number_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02)),
    limits = c(0, 100)
  ) +

  scale_fill_manual(
    values = c("in" = "#595959", "out" = "#CCCCCC"),
    labels = c("in" = "Inside tissue", "out" = "Outside tissue")
  ) +

  ylab("Algae with caspase positive symbiocytes") +
  xlab("") +

  # Theme customization
  theme_classic(base_size = 6) +
  theme(
    text = element_text(),
    axis.title.y = element_text(size = 6, margin = margin(r = 2)),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.25),
    axis.ticks = element_line(size = 0.25),
    legend.title = element_blank(),
    legend.text = element_text(size = 6, color = "black"),
    legend.key.size = unit(.3, "cm"),
    legend.position = c(1.4, 1.0),  # outside top-right
    legend.justification = "top",
    legend.box = "none",
    plot.margin = margin(2, 2, 2, 2, "pt")
  )
```

    ## Warning in geom_col(color = "black", width = 0.45, size = 0.25): Ignoring
    ## unknown parameters: `size`

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggsave("Fig2G.pdf",
       path = "Figures",
       width = 1.0, height = 1.08, units = "in", dpi = 600)
```

\#Plotting for Figure S8D

``` r
library(dplyr)
library(ggplot2)
library(boot)

# Load data
all_data <- read.csv(
  "Data/FigS8D.csv",
  header = TRUE
)

# Ensure 'location' is a factor
all_data$location <- factor(all_data$location)

sum_in <- sum(all_data$value[all_data$location == "in"], na.rm = TRUE)
sum_in
```

    ## [1] 11.58866

``` r
sum_out <- sum(all_data$value[all_data$location == "out"], na.rm = TRUE)
sum_out
```

    ## [1] 13.34927

``` r
summary_data <- all_data %>%
  group_by(location) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(percent = total_value / sum(total_value) * 100)


ggplot(summary_data, aes(x = " ", y = percent, fill = location)) +
  geom_col(color = "black", width = 0.45, size = 0.25) +


  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_stack(vjust = 0.5),
    color = "black", size = 3
  ) +

  scale_y_continuous(
    labels = scales::number_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02)),
    limits = c(0, 100)
  ) +

  scale_fill_manual(
    values = c("in" = "#595959", "out" = "#CCCCCC"),
    labels = c("in" = "Inside tissue", "out" = "Outside tissue")
  ) +

  ylab("Algae with caspase positive symbiocytes") +
  xlab("") +

  # Theme customization
  theme_classic(base_size = 6) +
  theme(
    text = element_text(),
    axis.title.y = element_text(size = 6, margin = margin(r = 2)),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.25),
    axis.ticks = element_line(size = 0.25),
    legend.title = element_blank(),
    legend.text = element_text(size = 6, color = "black"),
    legend.key.size = unit(.3, "cm"),
    legend.position = c(1.4, 1.0),  # outside top-right
    legend.justification = "top",
    legend.box = "none",
    plot.margin = margin(2, 2, 2, 2, "pt")
  )
```

    ## Warning in geom_col(color = "black", width = 0.45, size = 0.25): Ignoring
    ## unknown parameters: `size`

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggsave("FigS8D.pdf",
       path = "Figures",
       width = 1.0, height = 1.08, units = "in", dpi = 600)
```

\#Plotting for Figure 3B

``` r
#Plotting for Figure 3B

# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Load data
all_data <- as.data.frame(read.csv(
  file = "Data/Fig3B.csv",
  header = TRUE
))

all_data <- all_data %>%
mutate(treatment_num = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(treatment)))))

colors <- c(heat = "#E41A1C", control = "#377EB8")

agg <- all_data %>%
filter(type %in% c("a01", "b01"),
stress %in% c("heat", "control")) %>%
group_by(type, stress, treatment_num) %>%
summarize(
mean_value = mean(value, na.rm = TRUE),
n          = sum(!is.na(value)),
sem_value  = sd(value, na.rm = TRUE) / sqrt(n),
.groups = "drop"
) %>%
arrange(type, stress, treatment_num)

rep_data <- all_data %>%
filter(type %in% c("a01", "b01"),
stress %in% c("heat", "control")) %>%
group_by(type, stress, experiment, Replicate, treatment_num) %>%
summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
arrange(type, stress, experiment, Replicate, treatment_num)

all_data <- all_data %>%
mutate(type = factor(type, levels = rev(sort(unique(type)))))

agg <- agg %>%
mutate(type = factor(type, levels = rev(sort(unique(type)))))

rep_data <- rep_data %>%
mutate(type = factor(type, levels = rev(sort(unique(type)))))

ggplot() +
geom_ribbon(
data = agg,
aes(x = treatment_num, ymin = mean_value - sem_value, ymax = mean_value + sem_value, fill = stress),
alpha = 0.20, colour = NA, show.legend = FALSE, na.rm = TRUE
) +
geom_line(
data = agg,
aes(x = treatment_num, y = mean_value, colour = stress, group = stress),
size = 1
) +
geom_point(
data = agg,
aes(x = treatment_num, y = mean_value, colour = stress),
size = 2
) +
facet_wrap(~ type, ncol = 1) +
scale_colour_manual(values = colors) +
scale_fill_manual(values = colors) +
scale_x_continuous(breaks = sort(unique(na.omit(agg$treatment_num)))) +
labs(
y = "Log of alga movement relative to control at each timepoint",
x = "Time since initiation of heat (hours)",
colour = "Stress"
) +
theme_minimal() +
theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_rect(fill = "white", colour = NA),
axis.line = element_line(color = "black"),
axis.ticks = element_line(color = "black"),
axis.text = element_text(color = "black"),
axis.title = element_text(color = "black")
)
```

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
ggsave("Fig3B.pdf",
     path = "Figures",
     width = 5, height = 5, units = "in", dpi = 300)
```

\#Stats for Figure 3B

``` r
#Stats for Figure 3B

library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.1     ✔ readr     2.2.0
    ## ✔ lubridate 1.9.5     ✔ tibble    3.3.1
    ## ✔ purrr     1.2.2     ✔ tidyr     1.3.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ readr::col_factor() masks scales::col_factor()
    ## ✖ purrr::discard()    masks scales::discard()
    ## ✖ rstatix::filter()   masks dplyr::filter(), stats::filter()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rstatix)
library(ggpubr)

stat.test1 <- rep_data %>%
  filter(type == 'b01') %>%
  group_by(treatment_num) %>%
  t_test(value ~ stress) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test1
```

    ## # A tibble: 6 × 11
    ##   treatment_num .y.   group1  group2    n1    n2 statistic    df       p   p.adj
    ##           <dbl> <chr> <chr>   <chr>  <int> <int>     <dbl> <dbl>   <dbl>   <dbl>
    ## 1             0 value control heat      12     5    -0.257  4.93 8.08e-1 0.808  
    ## 2            12 value control heat      12     5    -8.39   5.47 2.53e-4 0.00152
    ## 3            24 value control heat      12     5    -7.05   4.57 1.28e-3 0.00384
    ## 4            48 value control heat      12     5    -3.72   4.10 1.96e-2 0.0392 
    ## 5            72 value control heat      12     5    -0.800  4.18 4.67e-1 0.560  
    ## 6            96 value control heat      12     5     1.06   4.96 3.40e-1 0.510  
    ## # ℹ 1 more variable: p.adj.signif <chr>

``` r
stat.test2 <- rep_data %>%
  filter(type == 'a01') %>%
  group_by(treatment_num) %>%
  t_test(value ~ stress) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test2
```

    ## # A tibble: 6 × 11
    ##   treatment_num .y.   group1  group2    n1    n2 statistic    df       p   p.adj
    ##           <dbl> <chr> <chr>   <chr>  <int> <int>     <dbl> <dbl>   <dbl>   <dbl>
    ## 1             0 value control heat       6     5    0.0531  8.81 9.59e-1 9.59e-1
    ## 2            12 value control heat       6     5  -11.0     8.46 2.70e-6 1.62e-5
    ## 3            24 value control heat       6     5   -7.97    7.03 9.17e-5 2.75e-4
    ## 4            48 value control heat       6     5   -1.24    7.92 2.51e-1 3.77e-1
    ## 5            72 value control heat       6     5   -0.266   4.71 8.02e-1 9.59e-1
    ## 6            96 value control heat       6     5    1.46    6.33 1.92e-1 3.77e-1
    ## # ℹ 1 more variable: p.adj.signif <chr>

\#Plotting for Figure 3D

``` r
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(ggpubr)   # for ggarrange(); install.packages("ggpubr") if needed

# x-axis order
treatment_order <- c("Ambient_b01", "Heat_b01", "Ambient_a01", "Heat_a01")

# Colors for experiments
exp_colors <- c(
  "1" = "#009E73", "2" = "#D55E00", "3" = "#56B4E9", "4" = "#E69F00",
  "5" = "#000000", "6" = "#0072B2", "7" = "#CC79A7", "8" = "#999999"
)

# ---- Load and clean one data file ----
load_data <- function(file) {
  all_data <- read.csv(file = file, header = TRUE) %>%
    mutate(
      treatment  = str_trim(as.character(treatment)),
      experiment = factor(experiment)
    ) %>%
    filter(!is.na(treatment), treatment != "", !is.na(value))

  unmatched <- setdiff(unique(all_data$treatment), treatment_order)
  if (length(unmatched) > 0) {
    warning("In ", file, ", these treatments are not in treatment_order and were dropped: ",
            paste(unmatched, collapse = ", "))
  }

  all_data %>%
    filter(treatment %in% treatment_order) %>%
    mutate(
      treatment         = factor(treatment, levels = treatment_order),
      treatment_num     = as.integer(treatment),
      treatment_wrapped = str_wrap(as.character(treatment), width = 8)
    )
}

# ---- Function to build one bar plot ----
make_bar_plot <- function(file, y_label, y_limits = NULL) {

    all_data <- load_data(file)

  # Means and standard errors per treatment
  bar_means <- all_data %>%
    group_by(treatment, treatment_num, treatment_wrapped) %>%
    summarise(
      mean_val = mean(value, na.rm = TRUE),
      se       = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups  = "drop"
    ) %>%
    arrange(treatment_num)

  n_bars <- length(treatment_order)

  ggplot() +
    geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
             fill = "grey", color = "black", width = 0.9, linewidth = 0.2) +

    geom_errorbar(data = bar_means,
                  aes(x = treatment_num, ymin = mean_val - se, ymax = mean_val + se),
                  width = 0.2, linewidth = 0.2, color = "black") +

    geom_point(data = all_data,
               aes(x = treatment_num, y = value, color = experiment),
               position = position_jitter(width = 0.2), size = 1,
               alpha = 0.8, stroke = NA) +

    scale_x_continuous(
      breaks = bar_means$treatment_num,
      labels = bar_means$treatment_wrapped,
      limits = c(0.5, n_bars + 0.5),
      expand = c(0, 0)
    ) +

    scale_y_continuous(limits = y_limits,
                       expand = expansion(mult = c(0, 0.05))) +

    scale_color_manual(values = exp_colors) +

    theme_classic(base_size = 6) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 7),
      axis.text.x  = element_text(size = 6, color = "black"),
      axis.text.y  = element_text(size = 6, color = "black"),
      axis.line    = element_line(color = "black", linewidth = 0.3),
      axis.ticks   = element_line(linewidth = 0.3),
      plot.margin  = margin(2, 2, 2, 2, "pt"),
      legend.position = "none"
    ) +
    ylab(y_label) +
    coord_cartesian(clip = "off")
}

# ---- Build the two panels ----
p_top <- make_bar_plot(
  file     = "Data/Fig3Dtop.csv",
  y_label  = "Top panel label",   # <-- set your y-axis label
  y_limits = NULL                 # <-- e.g. c(-0.01, 100); NULL = automatic
)

p_bottom <- make_bar_plot(
  file     = "Data/Fig3Dbottom.csv",
  y_label  = expression("Volume of chlorophyll (" * mu * m^3 * ")"),
  y_limits = c(-0.01, 650)
)

# ---- Stack and save ----
fig3d <- ggarrange(p_top, p_bottom, ncol = 1, nrow = 1 + 1, align = "v")

fig3d
```

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggsave("Fig3D.pdf", plot = fig3d, path = "Figures",
       width = 2, height = 2.8, units = "in", dpi = 300)
```

\#Stats for Figure 3D

``` r
# ---- Statistics ----
run_stats <- function(file) {
  all_data <- load_data(file)
  fit <- aov(value ~ treatment, data = all_data)
  cat("\n=====", file, "=====\n")
  print(anova(fit))
  print(TukeyHSD(fit))
}

run_stats("Data/Fig3Dtop.csv")
```

    ## 
    ## ===== Data/Fig3Dtop.csv =====
    ## Analysis of Variance Table
    ## 
    ## Response: value
    ##            Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## treatment   3 3099857 1033286   123.7 < 2.2e-16 ***
    ## Residuals 375 3132451    8353                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = value ~ treatment, data = all_data)
    ## 
    ## $treatment
    ##                               diff        lwr        upr     p adj
    ## Heat_b01-Ambient_b01    -120.06207 -155.44785 -84.676293 0.0000000
    ## Ambient_a01-Ambient_b01  101.39577   63.41134 139.380198 0.0000000
    ## Heat_a01-Ambient_b01      73.94685   38.06266 109.831045 0.0000011
    ## Ambient_a01-Heat_b01     221.45784  187.54973 255.365947 0.0000000
    ## Heat_a01-Heat_b01        194.00892  162.47135 225.546491 0.0000000
    ## Heat_a01-Ambient_a01     -27.44892  -61.87684   6.979008 0.1691612

``` r
run_stats("Data/Fig3Dbottom.csv")
```

    ## 
    ## ===== Data/Fig3Dbottom.csv =====
    ## Analysis of Variance Table
    ## 
    ## Response: value
    ##            Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## treatment   3  475338  158446  19.516 4.808e-11 ***
    ## Residuals 186 1510075    8119                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = value ~ treatment, data = all_data)
    ## 
    ## $treatment
    ##                               diff         lwr       upr     p adj
    ## Heat_b01-Ambient_b01    -119.69175 -173.376062 -66.00744 0.0000002
    ## Ambient_a01-Ambient_b01   32.09200   -7.832664  72.01666 0.1621990
    ## Heat_a01-Ambient_b01      45.77888   -7.117839  98.67561 0.1154560
    ## Ambient_a01-Heat_b01     151.78375   96.313467 207.25403 0.0000000
    ## Heat_a01-Heat_b01        165.47064  100.041002 230.90027 0.0000000
    ## Heat_a01-Ambient_a01      13.68689  -41.021526  68.39530 0.9159442

\#Plotting for Figure S7B

``` r
library(dplyr)
library(ggplot2)
library(stringr)  # Needed for str_wrap()
library(scales)

# Load data
all_data <- as.data.frame(read.csv(
  file = "Data/FigS7B.csv",
  header = TRUE
))

# Convert experiment to factor
all_data$experiment <- factor(all_data$experiment)

# Convert treatment to factor
all_data <- all_data %>%
  mutate(treatment = factor(treatment)) %>%
  mutate(
    treatment_wrapped = str_wrap(as.character(treatment), width = 8),
    treatment_num = as.numeric(treatment)
  )

# Recalculate bar_means after filtering
bar_means <- all_data %>%
  group_by(treatment, treatment_num, treatment_wrapped) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

# Plot
ggplot() +
  geom_col(data = bar_means, aes(x = treatment_num, y = mean_val),
           fill = "grey", color = "black", width = .9, size = 0.2) +

  geom_point(data = all_data,
             aes(x = treatment_num, y = value, color = experiment),
             position = position_jitter(width = 0.2), size = 1.4, alpha = 0.8, stroke = NA) +

  stat_summary(data = all_data, aes(x = treatment_num, y = value),
  fun.data = mean_se, geom = "errorbar", width = 0.2,
  size = 0.2, color = "black") +
  
  scale_x_continuous(
    breaks = sort(unique(all_data$treatment_num)),
    labels = bar_means$treatment_wrapped
  ) +

  scale_y_continuous(
  limits = c(0, 1.0),
  expand = expansion(mult = c(0, 0.05)),
  labels = function(x) x * 100  # Remove percent sign
) +

  scale_color_manual(values = c("1" = "#0A9F9D", "2" = "#E54E21")) +

  theme_classic(base_size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.line = element_line(color = 'black', size = 0.3),
    axis.ticks = element_line(size = 0.3),
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.position = "none"
  ) +
  ylab("Expelled algae") 
```

    ## Warning in geom_col(data = bar_means, aes(x = treatment_num, y = mean_val), :
    ## Ignoring unknown parameters: `size`

![](Kowalewski_bleaching_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# Save the plot
ggsave("FigS7B.pdf",
     path = "Figures",
     width = 2, height = 1.8, units = "in", dpi = 300)
```

\#Stats for Figure S7B

``` r
a <- all_data %>%
  filter(treatment_wrapped == 'Symbiotic\nalgae') %>%
  pull(value)


b <- all_data %>%
  filter(treatment_wrapped == 'Free\nalgae') %>%
  pull(value)

t.test(a,b, var.equal=TRUE, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  a and b
    ## t = 6.3832, df = 9, p-value = 0.0001278
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2959058 0.6207679
    ## sample estimates:
    ## mean difference 
    ##       0.4583369
