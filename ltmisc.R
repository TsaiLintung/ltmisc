library(ggplot2)

mytheme <- theme_bw() + theme(
    text = element_text(size = 18, family = "Times New Roman"),
    legend.position = "bottom",

    # plot.margin = margin(c(0,0,0,0)),
    axis.title = element_text(
        size = 18, family = "Times New Roman",
        margin = margin(t = 50, r = 50, b = 50, l = 50, unit = "pt")
    ),
    plot.title = element_textbox_simple(
        size = 18, family = "Times New Roman",
        margin = margin(20, 0, 10, 0)
    )
) # right top bot left

label <- function(x, ll = LABEL_LOOKUP) {
    # usage: label(c("total_var", "ss_sx"))
    x <- x |> str_trim()
    x <- fifelse(x %in% names(ll), ll[x], gsub("_", " ", x))
    x <- x |> to_sentence()
    return(x)
}

to_sentence <- function(string, abb = ABB) {
    if (string == "") {
        return("")
    }
    words <- strsplit(string, " ")[[1]]
    if (!(str_to_upper(words[1]) %in% abb)) {
        words[1] <- tools::toTitleCase(words[1])
    } else {
        words[1] <- str_to_upper(words[1])
    }
    return(paste(words, collapse = " "))
}
to_sentence <- Vectorize(to_sentence)


fnum <- function(x, sif = 3) {
  as.character(ifelse(x == 0, 0, ifelse(x == round(x), formatC(x, big.mark = ",", format = "f", digits = 0),
    ifelse(abs(x) < 10,
      ifelse(abs(x) < 0.001, formatC(x, format = "e", digits = sif), # very small number use sci
        format(round(x, digits = sif), nsmall = sif)
      ), # small number
      formatC(round(x, 1), big.mark = ",", format = "f", digits = 2)
    )
  ))) |>
    str_remove_all(" ") # remove space manually, not sure why
} 
fnum <- Vectorize(fnum)