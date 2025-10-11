# general formatting ------

getresult <- function(coef, sd, ci = FALSE, newline = FALSE, star = FALSE) { 
  qn <- qnorm(0.975)

  if (ci) {
    se_part <- paste0(" [", fnum(coef - qn * sd), ", ", fnum(coef + qn * sd), "]")
  } else {
    se_part <- paste0(" (", fnum(sd), ")")
  }

  if (star) {
    coef_part <- paste0(fnum(coef), getstar(coef, sd))
  } else {
    coef_part <- fnum(coef)
  }

  if (newline) {
    result <- paste0("\\makecell{", coef_part, "\\\\", se_part, "}")
  } else {
    result <- paste0(coef_part, se_part)
  }
  result[is.na(coef) | is.na(sd)] <- ""
  return(result)
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

label <- function(x, ll = LABEL_LOOKUP) {
    #usage: label(c("total_var", "ss_sx"))
    x <- x |> str_trim()
    x <- fifelse(x %in% names(ll), ll[x], gsub("_", " ", x))
    x <- x |> to_sentence()
    return(x)
}

fnum <- function(x, sif = 3) {
  as.character(ifelse(x == 0, 0, ifelse(x == round(x), formatC(x, big.mark = ",", format = "f", digits = 0),
    ifelse(abs(x) < 10,
      ifelse(abs(x) < 0.001, formatC(x, format = "e", digits = sif), # very small number use sci
        format(round(x, digits = sif), nsmall = sif)
      ), # small number
      formatC(round(x, 1), big.mark = ",", format = "f", digits = 1)
    )
  ))) |>
    str_remove_all(" ") # remove space manually, not sure why
} 
fnum <- Vectorize(fnum)

# dynamic number ------

add_value <- function(vartable, name, value){
    vartable <- rbind(vartable, data.table(name = name, 
        value = value))
    return(vartable)
}

add_alt_numbers <- function(vartable) {
  # add x100 ver
  pvartable <- copy(vartable)
  pvartable[, name := paste0(name, "_p")]
  pvartable[, value := value * 100]
  vartable <- vartable |> rbind(pvartable)

  # add abs ver
  absvartable <- copy(vartable)
  absvartable[, name := paste0(name, "_a")]
  absvartable[, value := abs(value)]
  vartable <- vartable |> rbind(absvartable)
  return(vartable)
}

replace_digits_with_alpha <- function(input_string) {
  digit_to_alpha <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
  chars <- strsplit(input_string, "")[[1]]
  replaced <- sapply(chars, function(char) {
    if (grepl("[0-9]", char)) {
      return(digit_to_alpha[as.numeric(char) + 1])
    } else {
      return(char)
    }
  })
  result <- paste0(replaced, collapse = "")
  return(result)
}
