# Use lme4 and lmerTest for statistics
library("lme4")
library("lmerTest")

# Source the modified version of stargazer
# I modified the stargazer source code so that it prints out the p values in scientific
# notation when they are very small.
source("stargazer_modified/stargazer.R")

# Do the contrast coding for each categorical variable
named.contr.sum<-function(x, ...) {
    if (is.factor(x)) {
        x <- levels(x)
    } else if (is.numeric(x) & length(x)==1L) {
        stop("cannot create names with integer value. Pass factor levels")
    }
    x<-contr.sum(x, ...)
    colnames(x) <- apply(x,2,function(x)names(x[x>0])
    )
    x
}

# Lets look at the interaction of of group and adverb for each structure
run_model <- function(stats_df, filename) {
    mod <- lmer(Rating ~ Group.f * Structure.f * Adverb.f+ (1|PID) + (1|QID), data=stats_df)
    pvals <- list(summary(mod)$coefficients[, 5])
    class(mod) <- "lmerMod"

    # Write the model output to a file
    fileConn<-file(filename)

    writeLines(
        capture.output(stargazer(mod, title="Rating ~ Group * Structure + (1|PID) + (1|QID)",
                                 report=('vc*tp'), type='html', p=pvals)),
        fileConn
    )

    close(fileConn)
}

get_random_filename <- function() {
    # Get a random filename that has ten alphanumeric characters
    random_filename <- paste(sample(c(letters, LETTERS, 0:9), 10), collapse="")
    return(random_filename)
}

run_models_and_combine_reports <- function(stats_df, output_filename) {
    stats_df$Group.f = factor(stats_df$Group)
    stats_df$Adverb.f = factor(stats_df$Adverb)
    stats_df$Mente.f = factor(stats_df$Mente)
    stats_df$Adverb_Class.f = factor(stats_df$Adverb_Class)
    stats_df$Structure.f = factor(stats_df$Structure)

    # Using the normal, unnamed contrast coding
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    contrasts(stats_df$Mente.f) = named.contr.sum(stats_df$Mente.f)
    contrasts(stats_df$Adverb_Class.f) = named.contr.sum(stats_df$Adverb_Class.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)

    # Contrast code for the average within each group and structure
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file1 <- get_random_filename()
    run_model(stats_df, file1)

    # First relevel the Group
    stats_df$Group.f <- relevel(stats_df$Group.f, ref=(tail(levels(stats_df$Group.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file2 <- get_random_filename()
    run_model(stats_df, file2)

    # Then relevel the Structure
    stats_df$Structure.f <- relevel(stats_df$Structure.f, ref=(tail(levels(stats_df$Structure.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file3 <- get_random_filename()
    run_model(stats_df, file3)

    # Revel the group again
    stats_df$Group.f <- relevel(stats_df$Group.f, ref=(tail(levels(stats_df$Group.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file4 <- get_random_filename()
    run_model(stats_df, file4)

    # Revel the adverb
    stats_df$Adverb.f <- relevel(stats_df$Adverb.f, ref=(tail(levels(stats_df$Adverb.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file5 <- get_random_filename()
    run_model(stats_df, file5)

    # Revel the group
    stats_df$Group.f <- relevel(stats_df$Group.f, ref=(tail(levels(stats_df$Group.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file6 <- get_random_filename()
    run_model(stats_df, file6)

    # Revel the struct
    stats_df$Structure.f <- relevel(stats_df$Structure.f, ref=(tail(levels(stats_df$Structure.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file7 <- get_random_filename()
    run_model(stats_df, file7)

    # Revel the group
    stats_df$Group.f <- relevel(stats_df$Group.f, ref=(tail(levels(stats_df$Group.f), n=1)))
    contrasts(stats_df$Group.f) = named.contr.sum(stats_df$Group.f)
    contrasts(stats_df$Structure.f) = named.contr.sum(stats_df$Structure.f)
    contrasts(stats_df$Adverb.f) = named.contr.sum(stats_df$Adverb.f)
    file8 <- get_random_filename()
    run_model(stats_df, file8)

    # Run the combining python script
    system(
        paste('python3 ../combine_model_outputs.py', file1, file2, file3, file4, file5, file6, file7, file8, output_filename, sep=' ')
    )
}

stats_df <- read.csv(file='ajt_data.csv')

# ------------------------- Analysis 1: NS group + 3 HS groups by proficiency -------------------------
output_filename <- "model_outputs/group-adverbs-structures-in-ns-and-hs-groups-model_output.html"

analysis_1 <- stats_df

# drop rows where the group is "High", "Int", or "Low" to remove L2 participants
analysis_1 <- analysis_1[!analysis_1$Group %in% c("High", "Int", "Low"),]

run_models_and_combine_reports(analysis_1, output_filename)

# ------------------------- Analysis 2: NS vs. HS vs. L2 groups -------------------------
output_filename <- "model_outputs/group-adverbs-structures-in-ns-and-hs-and-l2-model_output.html"

analysis_2 <- stats_df

# rename the groups where "HS Low", "HS Int", "HS High" are all renamed to "HS"
analysis_2$Group[analysis_2$Group %in% c("HS Low", "HS Int", "HS High")] <- "HS"

# rename the groups so "High", "Int", and "Low" are all renamed to L2
analysis_2$Group[analysis_2$Group %in% c("High", "Int", "Low")] <- "L2"

run_models_and_combine_reports(analysis_2, output_filename)

# ------------------------- Analysis 3: NS vs. 3 HS groups by proficiency vs 3 L2 groups by proficiency -------------------------
output_filename <- "model_outputs/group-adverbs-structures-in-all-groups-model_output.html"

analysis_3 <- stats_df

run_models_and_combine_reports(analysis_3, output_filename)

