full_df <- read.csv(file='ajt_data.csv')

make_plots <- function(df, group_string) {
  # loop through each adverb class
  for (adverb_class in unique(df$Adverb_Class)) {
    # make a plot
    filename = paste("proficiency_plots/", paste(gsub(" ", "_", group_string), adverb_class, sep="_"), ".png", sep="")
    png(filename, width = 450, height = 450)

    only_adverb_class_df <- df[df$Adverb_Class %in% c(adverb_class),]
    plot(Rating ~ Proficiency, data = only_adverb_class_df, type = 'n')

    # create a vector to store model names
    model_names <- unique(only_adverb_class_df$Structure)

    # create an empty vector to store the colors for the legend
    legend_colors <- vector("character", length = length(model_names))

    for (i in 1:length(model_names)) {
      structure <- model_names[i]
      only_structure_df <- only_adverb_class_df[only_adverb_class_df$Structure %in% c(structure),]

      mod <- lm(Rating ~ Proficiency, data = only_structure_df)

      newx <- seq(min(only_structure_df$Proficiency), max(only_structure_df$Proficiency), length.out=100)

      preds <- predict(mod, newdata = data.frame(Proficiency=newx), interval = 'confidence')

      # confidence interval
      polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)

      # model
      abline(mod, col = rainbow(length(model_names))[i])

      # intervals
      lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
      lines(newx, preds[ ,2], lty = 'dashed', col = 'red')

      # store colors for legend
      legend_colors[i] <- rainbow(length(model_names))[i]
    }

    # Adjust the plot margins to make space for the legend
    par(mar=c(5.1, 4.1, 4.1, 6.6))

    # Add extra space to right of plot area; change clipping to figure
    par(xpd=TRUE)
    legend("bottomright", legend = model_names, col = legend_colors, lwd = 1)
    title(main = paste(adverb_class, "adverbs in", group_string))

    # finish saving the plot
    dev.off()
    par(xpd=FALSE)
  }
}

# filter to only heritage speakers
hs_df <- full_df[full_df$Group %in% c("HS Low", "HS Int", "HS High"),]
make_plots(hs_df, "heritage speakers")

l2_df <- full_df[full_df$Group %in% c("Low", "Int", "High"),]
make_plots(l2_df, "L2 learners")
