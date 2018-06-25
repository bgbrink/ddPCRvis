FROM bioconductor/release_core2:R3.5.0_Bioc3.7

MAINTAINER Benedikt G Brink "bbrink@cebitec.uni-bielefeld.de"

# dpkg fix
RUN sudo dpkg --clear-avail

# Install dependencies and Download and install shiny server
RUN apt-get update && apt-get install -y \
    apt-utils \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libxt-dev \
    libgsl0-dev \
    libgeos-dev ed \
    automake autoconf libtool \
    subversion \
    git \
    build-essential && \
    wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'shinyjs', 'rhandsontable', 'jsonlite', 'devtools', 'R.utils', 'roxygen2', 'plotrix', 'clue', 'ggplot2', 'openxlsx', 'plotly'), repos='https://cran.rstudio.com/')" && \
    R -e "biocLite(c('ddPCRclust'))" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    git clone --recursive https://github.com/bgbrink/ddPCRvis.git /srv/shiny-server/ddPCRvis && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
