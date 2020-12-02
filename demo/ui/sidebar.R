argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "md",
  side = "left",
  id = "my_sidebar",
  brand_url = "#",
  brand_logo = "https://i.ibb.co/RHsWXP5/logo.png",
  argonSidebarHeader(title = "Menu"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "data",
      icon = argonIcon(name = "bullet-list-67", color = "danger"),
      "Data Samples"
    ),
    argonSidebarItem(
      tabName = "result",
      icon = argonIcon(name = "tv-2", color = "info"),
      "Classification"
    )
    # argonSidebarItem(
    #   tabName = "tabs",
    #   icon = argonIcon(name = "planet", color = "warning"),
    #   "Tabs"
    # ),
    # argonSidebarItem(
    #   tabName = "alerts",
    #   icon = argonIcon(name = "bullet-list-67", color = "danger"),
    #   "Alerts"
    # ),
    # argonSidebarItem(
    #   tabName = "medias",
    #   icon = argonIcon(name = "circle-08", color = "success"),
    #   "Medias"
    # ),
    # argonSidebarItem(
    #   tabName = "items",
    #   icon = argonIcon(name = "ui-04", color = "pink"),
    #   "Other items"
    # ),
    # argonSidebarItem(
    #   tabName = "effects",
    #   icon = argonIcon(name = "atom", color = "black"),
    #   "CSS effects"
    # ),
    # argonSidebarItem(
    #   tabName = "sections",
    #   icon = argonIcon(name = "credit-card", color = "grey"),
    #   "Sections"
    # )
  )
)
