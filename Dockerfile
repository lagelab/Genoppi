FROM rocker/shiny
MAINTAINER Yu-Han Hsu (yuhanhsu@broadinstitute.org)

## install R package dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libgit2-dev \
    ## clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## install devtools package from CRAN
RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    devtools \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## install genoppi from GitHub
RUN Rscript -e "devtools::install_github('lagelab/Genoppi')" \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# install rstudion/httpuv to enable compatibility with google cloud run https://github.com/rstudio/shiny/issues/2455
RUN Rscript -e "devtools::install_github('rstudio/httpuv')" \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## copy app and configuration files into Docker image
COPY inst/shiny-examples/myapp /srv/shiny-server/
COPY inst/shiny-examples/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY inst/shiny-examples/shiny-server.sh /usr/bin/shiny-server.sh

## run app
CMD ["/usr/bin/shiny-server.sh"]

