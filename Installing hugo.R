install_hugo(
  version = "0.101.0"
)


blogdown::new_site(theme = "MarcusVirg/forty", sample = TRUE,
                   theme_example = TRUE, empty_dirs = TRUE, to_yaml = TRUE)

