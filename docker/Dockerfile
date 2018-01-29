FROM rocker/r-ver:3.4.3

MAINTAINER Jan Winter "jan.winter@dkfz.de"

#### LINUX DEPENDENCIES
RUN apt-get update && apt-get install -y  \
    wget \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    build-essential \
    libgd-dev \
    libexpat1-dev \
    libxml2-dev \
    git \
    libssl-dev \
    curl \
    libssl-dev \
    libtiff5-dev \
    htop \
    ghostscript \
    nginx \
    libtbb2 \
    libmariadbclient-dev

# Add TEX for report and R Markdown
RUN apt-get update && apt-get -y --no-install-recommends \
   install texlive texlive-xetex

# install the shiny server debian package from r-studio
RUN wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.3.838-amd64.deb -P /tmp/
RUN gdebi -n /tmp/shiny-server-1.5.3.838-amd64.deb && \
    rm -f /tmp/shiny-server-1.5.3.838-amd64.deb

COPY ./shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

#### R PACKAGES

# First we need devtools for all the installation of all further packages
RUN R -e 'install.packages("devtools", repos = "http://cloud.r-project.org/")'

# install all the packages we need from CRAN, Bioconductor and GitHub

RUN R -e 'devtools::install_version("RCurl", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("XML", repos = "http://cloud.r-project.org/")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite()'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("BiocParallel")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("rtracklayer")'

# We have to retrieve thise package as it is not available anymore, but required by some packages
RUN wget http://www.omegahat.net/XMLRPC/XMLRPC_0.3-0.tar.gz -P /tmp/
RUN R -e 'install.packages("/tmp/XMLRPC_0.3-0.tar.gz", repos = NULL, type="source")'

RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("RCytoscape")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("RamiGO")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("BiocGenerics")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("limma")'
RUN R -e 'devtools::install_version("RMySQL", repos = "http://cloud.r-project.org/")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("GenomicFeatures")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("Rqc")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("IRanges")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("ShortRead")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("KEGGREST")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("GenomicRanges")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("biomaRt")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("DESeq2")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("edgeR")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("Gviz")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("STRINGdb")'
RUN R -e 'devtools::install_version("dplyr", version = "0.7.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_github("jyyu/ScreenBEAM", ref = "d6204b3")'
RUN R -e 'devtools::install_version("shinydashboard", version = "0.5.3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("jsonlite", version = "1.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyBS", version = "0.61", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("highcharter", version = "0.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("seqinr", version = "3.3-6", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("openxlsx", version = "4.0.17", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("caTools", version = "1.17.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("reshape2", version = "1.4.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("MESS", version = "0.4-3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("igraph", version = "1.0.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("data.table", version = "1.10.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_github("jimhester/gmailr", ref = "d5ee3c2")' #OLD d5ee3c2 # new 59dac01
RUN R -e 'devtools::install_version("d3heatmap", version = "0.6.1.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("DEoptim", version = "2.2-3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("httr", version = "1.3.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("sgRSEA", version = "0.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("VennDiagram", version = "1.6.17", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("tidyverse", version = "1.2.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("htmltools", version = "0.3.6", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("DT", version = "0.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("sm", version = "2.2-5.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyjs", version = "0.9.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("markdown", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("knitr", version = "1.16", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shiny", version = "1.0.5", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("bookdown", version = "0.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("R.utils", version = "2.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyWidgets", repos = "http://cloud.r-project.org/")'



# cleaning up downloaded deb packages for keeping clean our docker image
RUN apt-get -qq clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# now install the reannotate-crispr PERL package, which is based on the CRISPR LIBRARY DESIGNER

ENV PERL_MM_USE_DEFAULT=1
RUN perl -MCPAN -e 'CPAN::Shell->install("Bundle::CPAN")'

# now install all the modules we need for crispr reannotator
RUN perl -MCPAN -e 'CPAN::Shell->install("Bio::DB::Fasta")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Bio::SeqIO")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Bio::Tools::GFF")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Scalar::Util")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Bio::SeqFeature::Generic")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Bio::Location::Split")'
RUN perl -MCPAN -e 'CPAN::Shell->install("JSON::XS")'
RUN perl -MCPAN -e 'CPAN::Shell->install("File::Slurp")'
RUN perl -MCPAN -e 'CPAN::Shell->install("List::MoreUtils")'
RUN perl -MCPAN -e 'CPAN::Shell->install("List::Util")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Archive::Zip")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Parallel::ForkManager")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Cwd")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Getopt::Long")'
RUN perl -MCPAN -e 'CPAN::Shell->install("File::Grep")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Text::Wrap")'
RUN perl -MCPAN -e 'CPAN::Shell->install("Unix::Processors")'

RUN git clone https://github.com/boutroslab/Supplemental-Material.git /tmp/Supplemental-Material
RUN cp -r /tmp/Supplemental-Material/Rauscher\&Heigwer_2016/crispr-reannotation /opt/
RUN chmod +x /opt/crispr-reannotation/reannotate_crispr.pl
env PATH /opt/crispr-reannotation:$PATH
RUN rm -rf /tmp/Supplemental-Material

# install intervaltree...another dependency for  reannotate-crispr
RUN cd /opt/crispr-reannotation/depends/Set-IntervalTree-0.10-OD; perl Makefile.PL; make; make test && make install

# Install Python NumPy and SciPy required for BAGEL
RUN apt-get update && apt-get install -y python-numpy python-scipy python-sympy

## Install Software packages, Download and Install them

# Bowtie 2
RUN wget http://www.dkfz.de/signaling/crispranalyzer/bowtie2-2.2.9-linux-x86_64.zip -P /tmp/
RUN unzip /tmp/bowtie2-2.2.9-linux-x86_64.zip -d /opt
ENV PATH=/opt/bowtie2-2.2.9:$PATH
RUN echo 'export PATH=/opt/bowtie2-2.2.9/:$PATH' >> /etc/profile

# Bowtie 1
RUN wget http://www.dkfz.de/signaling/crispranalyzer/bowtie-1.2.1.1-linux-x86_64.zip -P /tmp/
RUN unzip /tmp/bowtie-1.2.1.1-linux-x86_64.zip -d /opt
ENV PATH=/opt/bowtie-1.2.1.1/:$PATH
RUN echo 'export PATH=/opt/bowtie-1.2.1.1/:$PATH' >> /etc/profile


# install MAGeCK from Sourceforge
RUN wget http://www.dkfz.de/signaling/crispranalyzer/mageck-0.5.5.tar.gz -P /tmp/
#COPY ./mageck-0.5.5.tar.gz /tmp/
RUN cd /tmp/; tar xvf ./mageck-0.5.5.tar.gz; cd /tmp/mageck-0.5.5; python setup.py install
RUN rm -rf /tmp/mageck-0.5.5 /tmp/mageck-0.5.5.tar.gz

### RUST-based FASTQ Extraction and SAM Extraction
# PERL-based backup variants are included in CRISPRAnalyzeR source

# Install RUST compiler
RUN \
    curl https://sh.rustup.rs > /tmp/sh.rustup.rs && \
         chmod +x /tmp/sh.rustup.rs && \
           /tmp/sh.rustup.rs -y && \
 rm /tmp/sh.rustup.rs
 
ENV PATH=/root/.cargo/bin:$PATH
 
# compile CRISPRAnalyzer mapper and extractor from source
RUN \
     git clone https://github.com/OliPelz/fastq_extractor_proof_of_principle.git \
     /tmp/crispranalyzer-rust-tools
# target dir for compiled executables
RUN \
     mkdir -p /opt/crispranalyzer-tools
 
# source cargo env file so we can use it
RUN \
     cd /tmp/crispranalyzer-rust-tools/extractor_in_RUST/fastq_parser && \
     cargo build --release && \
     cp ./target/release/fastq_parser /opt/crispranalyzer-tools/ && \
     chmod +x /opt/crispranalyzer-tools/fastq_parser
 
RUN \
    cd /tmp/crispranalyzer-rust-tools/sam_mapper_in_RUST/sam_mapper/ && \
    cargo build --release && \
    cp ./target/release/sam_mapper /opt/crispranalyzer-tools/ && \
    chmod +x /opt/crispranalyzer-tools/sam_mapper 

ENV PATH=/opt/crispranalyzer-tools:$PATH 
RUN echo 'export PATH=/opt/crispranalyzer-tools/:$PATH' >> /etc/profile

### CRISPRAnalyzeR FROM SOURCE

## Now we add CRISPRAnalyzeR code directly from Github Repository
ARG CACHEBUST=1
# We use CACHEBUST=1 arg to FORCE docker to start from here without caching
# This can be achieved by a build argument as described here : https://github.com/moby/moby/issues/1996#issuecomment-185872769
# docker build --rm --label crispranalyzer --build-arg CACHEBUST=$(date +%s) -t boutroslab/crispranalyzer:latest

# install CRISPRAnalyzeR
RUN mkdir /srv/shiny-server/CRISPRAnalyzeR 
RUN git clone --depth=1 https://github.com/boutroslab/CRISPRAnalyzeR.git /tmp/CRISPRAnalyzeR
RUN cp -r /tmp/CRISPRAnalyzeR/source/* /srv/shiny-server/CRISPRAnalyzeR/ 

# Generate Directories
RUN mkdir /srv/shiny-server/CRISPRAnalyzeR2 \
	 /srv/shiny-server/CRISPRAnalyzeR3 \
	 /srv/shiny-server/CRISPRAnalyzeR4 \
	 /srv/shiny-server/CRISPRAnalyzeR5
	 
RUN cp -r /srv/shiny-server/CRISPRAnalyzeR/multiuser/* /srv/shiny-server/CRISPRAnalyzeR2 && \
	 cp -r /srv/shiny-server/CRISPRAnalyzeR/multiuser/* /srv/shiny-server/CRISPRAnalyzeR3 && \
	 cp -r /srv/shiny-server/CRISPRAnalyzeR/multiuser/* /srv/shiny-server/CRISPRAnalyzeR4 && \
	 cp -r /srv/shiny-server/CRISPRAnalyzeR/multiuser/* /srv/shiny-server/CRISPRAnalyzeR5

# take CSS and web files
RUN cp -r /srv/shiny-server/CRISPRAnalyzeR/www /srv/shiny-server/CRISPRAnalyzeR2 && \
	 cp -r /srv/shiny-server/CRISPRAnalyzeR/www /srv/shiny-server/CRISPRAnalyzeR3 && \
	 cp -r /srv/shiny-server/CRISPRAnalyzeR/www /srv/shiny-server/CRISPRAnalyzeR4 && \
	 cp -r /srv/shiny-server/CRISPRAnalyzeR/www /srv/shiny-server/CRISPRAnalyzeR5

# we will run crispranalyzer as user shiny, so not as root!
RUN chown -R shiny:shiny /srv/shiny-server/CRISPRAnalyzeR
RUN chown -R shiny:shiny /srv/shiny-server/CRISPRAnalyzeR/userdata


# add R profile options
RUN echo 'setwd("/srv/shiny-server/CRISPRAnalyzeR")' >> /usr/local/lib/R/etc/Rprofile.site
RUN echo 'options(download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site


# NGINX (copy the config)
COPY nginx.conf /etc/nginx/nginx.conf

# shiny-server.conf
# shiny-server.conf will be automatically created with default values, which is totally fine

# Copy docker entrypoint and shiny-server.sh, in which is stated how we start shiny and nginx
# Moreover we copy the index.html to do a forward of the base directory to ./CRISPRAnalyzeR/, from which NGINX will do the rest
COPY docker-entrypoint.sh /
COPY index.html /srv/shiny-server
RUN chmod +x /docker-entrypoint.sh
RUN chmod +x /srv/shiny-server/index.html

EXPOSE 8000

# Add ENV for KiteMatic
# this ENV vars are required to be able to change these settings using Kitematic

ENV websockets_behind_proxy=FALSE
ENV verbose_logfiles=FALSE
ENV COSMIC_database="CosmicMutantExport.tsv"
ENV EnrichR_URL="http://amp.pharm.mssm.edu/Enrichr/"
ENV EnrichR=TRUE
ENV bowtie_threads=2
ENV proxy_url=
ENV proxy_port=
ENV max_upload=4096
ENV downloadlogs=TRUE

ENTRYPOINT ["/docker-entrypoint.sh"]
# finally run
CMD ["crispranalyzer"]
