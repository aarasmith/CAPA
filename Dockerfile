FROM rocker/r-ver:4.2.0

# system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    make \
    zlib1g-dev \
    libicu-dev \
    libpq-dev \
    libudunits2-dev \
    libgdal-dev\
    gdal-bin \
    libgeos-dev \
    libproj-dev
    


# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "ggplot2", \
              "shinyjs", \
              "dplyr", \
              "stringr", \
              "tidyr", \
              "RPostgres", \
              "sf", \
              "data.table", \
              "glue", \
              "RSQLite", \
              "writexl", \
              "dotenv", \
              "shinymanager" \              
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-05-23"\
          )'


RUN mkdir /root/CAPA

# copy the app directory into the image
#COPY ./CAPA/*.R /root/CAPA/
#COPY ./CAPA/*.RDS /root/CAPA/

# run app
CMD ["R", "-e", "shiny::runApp('/root/CAPA', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
