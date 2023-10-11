
# This is a parent function used to generate Quarto templates calling:
# - "dashboardClinicTemplate.qmd"
# - "dashboardManagerTemplate.qmd"
# - "dashboardHome.qmd"
# - "dashboardGoals.qmd"


clinic_map <- readr::read_csv("C:/Users/taylo/CPAL Dropbox/Benefits Delivery/04_Projects/WIC/WIC Dashboards/data/scriptData/clinic_map.csv")

# templates <- "C:/Users/taylo/CPAL Dropbox/Benefits Delivery/04_Projects/WIC/WIC Dashboards/templates/"
# templates <- "templates/"
templates <- ""

quarto::quarto_render(
  input = paste0(templates, "dashboardIndex.qmd"),
  output_file = "index.html"
)

for(goal_category in c("theme", "phase")){
  output_name <- gsub("\\W", "_", goal_category)

  quarto::quarto_render(
    input = paste0(templates, "dashboardGoals.qmd"),
    output_file = paste0(output_name, "s.html"),
    execute_params = list(goal_category = goal_category)
  )
}

for(clinic_name in clinic_map$Clinic){
  output_name <- gsub("\\W", "_", clinic_name) # replace any non-alphanumeric characters with underscore
  # navbar_links <- paste0(navbar_links,
  #                        "    - text: \"", clinic_name,
  #                        "\"\n      href: ", output_name, ".html\n")
  quarto::quarto_render(
    input = paste0(templates, "dashboardClinicTemplate.qmd"),
    output_file = paste0(output_name, ".html"),
    execute_params = list(clinic_name = clinic_name)
  )
}

for(manager_name in unique(clinic_map$Manager)){
  output_name <- gsub("\\W", "", manager_name) # replace any non-alphanumeric characters with underscore
  # navbar_links <- paste0(navbar_links,
  #                        "    - text: \"", clinic_name,
  #                        "\"\n      href: ", output_name, "_clinics.html\n")
  quarto::quarto_render(
    input = paste0(templates, "dashboardManagerTemplate.qmd"),
    output_file = paste0(output_name, "_clinics.html"),
    execute_params = list(manager_name = manager_name)
  )
}
