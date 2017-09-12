# R for Data Science Chapter 22
# Graphics for Communication with ggplot2
# Daniel J. Vera, Ph.D.
library(tidyverse)


# Label -------------------------------------------------------------------

# The easiest place to start when turning an exploratory graphic into an 
# expository graphic is with good labels.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    )
  )
  
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with",
      "engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception",
      "because of their light weight"
    ),
    caption = "Data from fueleconomy.gov"
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

# Exercises 28.2.1 on website:
# http://r4ds.had.co.nz/graphics-for-communication.html#exercises-71
# 1. Create one plot on the fuel economy data with customised title, 
# subtitle, caption, x, y, and colour labels.
ggplot(mpg, aes(x = reorder(class, hwy, median), y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Compact cars have > 10 Hwy MPG than Pickup Trucks",
    subtitle = "Comparing the median highway MPG in each class",
    caption = "Data from fueleconomy.gov",
    x = "Car Class",
    y = "Highway Miles per Gallon"
  )
  
# 2. The geom_smooth() is somewhat misleading because the hwy for large
# engines is skewed upwards due to the inclusion of lightweight sports 
# cars with big engines. Use your modelling tools to fit and display a 
# better model.
ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point(aes(color = class)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Subcompact cars show the gratest senstivity to engine size",
    caption = "Data from fueleconomy.gov"
  )
library(modelr)
mod <- lm(hwy ~ class, data = mpg)
mpg %>%
  add_residuals(mod) %>%
  ggplot(aes(displ, resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fuel efficiency decreases with engine size",
    subtitle = "Highway MPG for Cars After Subtracting Mean MPG of their Class",
    caption = "Data from fueleconomy.gov",
    x = "Highway MPG Relative to Class",
    y = "Engine Displacement"
  )

# 3. Take an exploratory graphic that you’ve created in the last month, 
# and add informative titles to make it easier for others to understand.

# Annotations -------------------------------------------------------------

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label = model), data = best_in_class)
# above is hard to read using geom_text so use geom_label and nudge_y:
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(
    aes(label = model),
    data = best_in_class,
    nudge_y = 2,
    alpha = 0.5
  )
# still have issues with overlapping labels so use ggrepel package:
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(
    aes(label = model),
    data = best_in_class
  )

class_avg <- mpg %>%
  group_by(class) %>%
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6, 
                            label.size = 0,
                            segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")

label <- mpg %>%
  summarize(
    displ = max(displ),
    hwy = max(hwy),
    label = paste(
      "Increasing engine size is \nrelated to",
      "decreasing fuel economy."
    )
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = paste(
    "Increasing engine size is \nrelated to",
    "decreasing fuel economy."
  )
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

"Increasing engine size related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()

# Exercises 28.2.1 on website:
# http://r4ds.had.co.nz/graphics-for-communication.html#exercises-72
# 1. Use geom_text() with infinite positions to place text at the four corners
# of the plot.

label <- tribble(
  ~displ, ~hwy, ~label, ~vjust, ~hjust,
  Inf,  Inf,    "Top right", "top", "right",
  Inf, -Inf,    "Bottom right", "bottom", "right",
 -Inf,  Inf,    "Top left", "top", "left",
 -Inf, -Inf,    "Bottom left", "bottom", "left"
)
label

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label, vjust = vjust, hjust = hjust), data = label)
# 2. Read the documentation for annotate(). How can you use it to add a 
# text label to a plot without having to create a tibble?
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  annotate("text", x = Inf, y = Inf,
           label = 
             "Increasing engine size is \nrelated to decreasing fuel economy",
           vjust = "top", hjust = "right")

# 3. How do labels with geom_text() interact with faceting? How can you add
# a label to a single facet? How can you put a different label in each 
# facet? (Hint: think about the underlying data.)
label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right",
            size = 2) +
  facet_wrap(~ class)

# To draw on one facet:
label <- tibble(
  displ = Inf,
  hwy = Inf,
  class = "2seater",
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right",
            size = 2) +
  facet_wrap(~ class)

label <- tibble(
  displ = Inf,
  hwy = Inf,
  class = unique(mpg$class),
  label = stringr::str_c("Label for ", class)
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right",
            size = 3) +
  facet_wrap(~ class)
# 4. What arguments to geom_label() control the appearance of the background
# box?
# label.padding: padding around label
# label.r: amount of rounding in the corners
# label.size: size of label border

# 5. What are the four arguments to arrow()? How do they work? Create a 
# series of plots that demonstrate the most important options.
# angle, length, ends, and type.

# Scales ------------------------------------------------------------------

# Axis Ticks and Legends
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes( color = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(
    NULL,
    breaks = presidential$start,
    date_labels = "'%y"
  )

# Legend Layout
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default
base + theme(legend.position = "none")


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) + 
  theme(legend.position = "bottom") + 
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4)
      )
  )
                                                                                                                                            
# Replacing a Scale
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

# for color blindness issues:
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))

# Viridis
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

# Exercises 28.2.1 on website:
# http://r4ds.had.co.nz/graphics-for-communication.html#exercises-73
# 1. Why doesn’t the following code override the default scale?

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()
# The default scale because the colors in geom_hex are set by the fill
# aesthetic, not the color aesthetic.
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()

# 2. What is the first argument to every scale? How does it compare to
# labs()?
# The first argument to every scale is the label for the scale. 
# It is equivalent to using the labs function.

# 3. Change the display of the presidential terms by:
#    a. Combining the two variants shown above.
#    b. Improving the display of the y axis.
#    c. Labelling each term with the name of the president.
#    d. Adding informative plot labels.
#    e. Placing breaks every 4 years (this is trickier than it seems!).

# Original:
presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))

library(lubridate)
years <- lubridate::make_date(seq(year(min(presidential$start)), 
                                  year(max(presidential$end)),
                                  by = 4), 1, 1)         

presidential %>%
  mutate(id = 33 + row_number(),
         name_id = stringr::str_c(name, " (", id, ")"),
         name_id = factor(name_id, levels = name_id)) %>%
  ggplot(aes(start, name_id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = name_id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue")) +
  scale_y_discrete(NULL) +
  scale_x_date(NULL, breaks = years, date_labels = "'%y") +
  theme(panel.grid.minor = element_blank())

# 4. Use override.aes to make the legend on the following plot easier 
# to see.

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20)

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20)  +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1)))  

# Zooming -----------------------------------------------------------------

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

suv <- mpg %>% 
  filter(class == "suv")
compact <- mpg %>%
  filter( class == "compact")

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()
ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
ggplot(compact,aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# Themes ------------------------------------------------------------------

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_bw()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_light()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_classic()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_linedraw()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_dark()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_minimal()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_gray()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  theme_void()

# Saving Your Plots -------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my-plot.pdf")