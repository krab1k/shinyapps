FROM rocker/shiny:4.3.3

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends libsodium23 default-jre libglpk-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN install2.r --error DT RCurl data.table dplyr emayili \
    gsubfn metacoder plotly rlang shiny shinyBS shinyFeedback \
    shinyWidgets shinybusy shinydashboard shinyjs sodium stringi \
    stringr taxize utf8 writexl shinyalert rhandsontable \
    readxl rJava xlsx imager showtext gtools forcats \
    reshape2 emmeans multcompView multcomp PMCMRplus && \
    rm -rf /tmp/downloaded_packages

COPY ./IntensityAnalyser /srv/shiny-server/IntensityAnalyser
COPY ./TeloBase /srv/shiny-server/TeloBase
COPY ./ScanToIntensity /srv/shiny-server/ScanToIntensity
