# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!


#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?
#' @return A new `CV_Printer` object.
create_CV_object <-  function(data_location) {

  cv <- list()
  
  excel_file <- paste0(data_location,"cv.xlsx")

  cv$entries_data <- readxl::read_xlsx(excel_file, "entries", skip = 1)
  #cv$activities   <- readxl::read_xlsx(excel_file, "activities", skip = 1)
  cv$skills       <- readxl::read_xlsx(excel_file, "skills", skip = 1)
  cv$text_blocks  <- readxl::read_xlsx(excel_file, "text", skip = 1)
  cv$contact_info <- readxl::read_xlsx(excel_file, "contact", skip = 1)
  cv$publications <- bib2df::bib2df(paste0(data_location, "cv.bib"))
  cv$acknowledgements <- bib2df::bib2df(paste0(data_location, "acknowledgements.bib"))
  cv$software <- bib2df::bib2df(paste0(data_location, "software.bib"))

  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

    return(date_year)
  }

  parse_dates <- function(dates){
    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", stringr::str_replace_all(description_bullets,c("\\{newline\\}" = "\n-", "\\{newlevel\\}" = "\n\t+"))), ""),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "N/A",
        no_start  & has_end ~ as.character(end),
        has_start & no_end  ~ paste("Current", "-", start),
        TRUE                ~ paste(end, "-", start)
      )
    ) %>%
    dplyr::arrange(desc(parse_dates(end))) %>%
    dplyr::group_by(section,timeline) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeline = ifelse(count != 1, "N/A", timeline)) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
  
  cv$acknowledgements %<>%
    dplyr::mutate(section = "Acknowledgement")
  
  cv$software %<>%
    dplyr::mutate(section = "Software")
  
  cv$publications %<>%
    dplyr::bind_rows(cv$acknowledgements) %>%
    dplyr::bind_rows(cv$software) %>%
    dplyr::mutate(
      month = factor(MONTH, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")),
      authors = sapply(AUTHOR, function(x) paste(x, collapse=", ")),
      dplyr::across(authors, stringr::str_replace, 'Quiroga, M.', "<b>Quiroga, M.</b>"),
      dplyr::across(authors, stringr::str_replace_all, 'ä', "&#228;"),
      dplyr::across(authors, stringr::str_replace_all, '’', "&#8217;"),
      dplyr::across(authors, stringr::str_replace_all, 'é', "&#233;"),
      dplyr::across(TITLE, stringr::str_replace_all, "[{}]", ""),
      section = factor(dplyr::case_when(
        !is.na(section) ~ section,
        CATEGORY == "ARTICLE" ~ "Article",
        CATEGORY == "TECHREPORT" ~ "Report",
        CATEGORY == "INCOLLECTION" ~ "Book chapter",
        CATEGORY == "MISC" ~ "Preprint",
        CATEGORY == "INPROCEEDINGS" ~ "Conference presentation",
        TRUE ~ "Unknown"
      ), levels = c("Article", "Report", "Book chapter", "Preprint", "Software", "Conference presentation", "Acknowledgement")),
      publisher = dplyr::case_when(
        !is.na(JOURNAL) ~ JOURNAL,
        !is.na(BOOKTITLE) ~ stringr::str_replace_all(BOOKTITLE, '\\{|\\}', ""),
        !is.na(PUBLISHER) ~ PUBLISHER,
        TRUE ~ NOTE
      ),
      detail = dplyr::case_when(
        (CATEGORY == "INPROCEEDINGS" | CATEGORY == "ARTICLE") & !is.na(NOTE) ~ NOTE,
        section == "Software" ~ ABSTRACT
      ),
      pages = dplyr::case_when(
        !is.na(NUMBER) & !is.na(PAGES) ~ paste0("*",VOLUME,"*(",NUMBER,")",", ",PAGES),
        !is.na(VOLUME) & !is.na(PAGES) ~ paste0("*",VOLUME,"*, ",PAGES),
        !is.na(PAGES) ~ PAGES,
        !is.na(VOLUME) ~ paste0("*",VOLUME,"*"),
        TRUE ~ ""
      ),
      doi = dplyr::case_when(
        !is.na(DOI) ~ DOI,
        !is.na(URL) ~ stringr::str_remove(URL, "https://doi.org/"),
        TRUE ~ ""
      ),
      end_year = extract_year(YEAR),
      url_title = dplyr::case_when(
        section == "Software" & !is.na(NOTE) ~ paste0('<a href=', NOTE, ' target="_blank">', TITLE, '</a>'),
        !is.na(URL) ~ paste0('<a href=', URL, ' target="_blank">', TITLE, '</a>'),
        TRUE ~ TITLE)
      ) %>%
    dplyr::rename(title = TITLE) %>%
    dplyr::arrange(desc(end_year), desc(month)) %>%
    dplyr::group_by(section,end_year) %>%
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::group_by(section) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(section) %>%
    dplyr::mutate(number = 1:dplyr::n()) %>%
    dplyr::mutate(
      timeline = dplyr::case_when(count != 1 ~ "N/A", TRUE ~ as.character(end_year)),
      number = dplyr::case_when(
        section=="Article" ~ paste0("\\* ", number, ". "), 
        TRUE ~ paste0(number, ". ")
        )) %>%
    dplyr::select(section, authors, end_year, URL, pages, title, publisher, doi, detail, timeline, url_title, ADDRESS, number) 
  
  cv$network <- cv$publications %>%
    dplyr::mutate(start_year = end_year) %>%
    dplyr::bind_rows(cv$entries_data) %>%
    dplyr::select(section, title, start_year, end_year, timeline, in_resume)
  
  cv
}


#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id, glue_template = "default"){
  if(glue_template == "default"){
    glue_template <- "
### {title}

{loc}

{institution}

{timeline}

{description_bullets}
\n\n\n"
  } else if(glue_template == "academic_cv"){
    glue_template <- "
### {title}

{loc}

{institution}

{timeline}
\n\n\n"
  }


  section_data <- dplyr::filter(cv$entries_data, section == section_id)

  print(glue::glue_data(section_data, glue_template))

  invisible(cv)
}



#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)

  cat(text_block)

  invisible(cv)
}



#' @description Construct a bar chart of skills
#' @param out_of The relative maximum for skills. Used to set what a fully filled in skill bar is.
print_skill_bars <- function(cv, skill_type = "technical", 
                             out_of = 5, bar_color = "#969696", bar_background = "#d9d9d9", glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{skill}</div>"
  }
  cv$skills %>%
    dplyr::filter(type == skill_type) %>%
    dplyr::mutate(width_percent = round(100*as.numeric(level)/out_of)) %>%
    glue::glue_data(glue_template) %>%
    print()

  invisible(cv)
}

print_skill_list <- function(cv, skill_type = "key_skill"){
  cv$skills %>% 
    dplyr::filter(type == skill_type) %>% 
    glue::glue_data(
      "- <i class='fa fa-{level}'></i> {skill}"
    ) %>% 
    print()
  invisible(cv)
}


#' @description Contact information section with icons
print_contact_info <- function(cv){
  glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}'></i> {contact}"
  ) %>% print()

  invisible(cv)
}

#' @description Print publications
#' @param type of publication in the `CATEGORY` column of the `publications` table
print_publications <- function(cv, type="Article"){
  article_section <- "{authors} ({end_year}). {title}. *{publisher}* {pages} <a href={URL} target='_blank'>{doi}</a>"
  
  if(type == "Report"){
    glue_section <- "{authors} ({end_year}). {url_title}. *{publisher}*"
  } else if(type == "Conference presentation"){
    glue_section <- "{authors} {url_title}. *{detail}*. {ADDRESS}"
  } else if(type == "Software"){
    glue_section <- "{url_title} ({end_year}). {detail}. {URL}"
  } else if(type == "Article"){
    glue_section <- paste0(article_section, " ({detail})")
  } else {
    glue_section <- article_section
  }
  
  type = stringr::str_split(type, "\\|")[[1]]
  publication_data <- dplyr::filter(cv$publications, section %in% type)
  
  glue_template <- paste0("
###

__{number}__ ", glue_section,"

N/A

{timeline}

\n\n\n")
  
  print(glue::glue_data(publication_data, glue_template))
  
  invisible(cv)

}

#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `activities` table
print_activity <- function(cv, section_id){
  glue_template <- "
###

<b>{event}</b>. {activity}.

{loc}

{timeline}

\n\n\n"

  section_data <- dplyr::filter(cv$activities, section == section_id)
  
  print(glue::glue_data(section_data, glue_template))
  
  invisible(cv)
}
