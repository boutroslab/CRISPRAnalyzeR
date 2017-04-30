FROM rocker/r-ver:3.3.2

MAINTAINER Oliver Pelz "o.pelz@dkfz.de"

#### things we need for the crispranalyzer package
#### and for the crispr reannotator
#### and another deb pkgs we later need for the R libraries to compile or run
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
    htop
    
RUN apt-get update && apt-get install -y ghostscript

# again some more things we need to run the crispranalyzer package
RUN apt-get update && apt-get -y --no-install-recommends \
   install texlive texlive-xetex

# install the shiny server debian package from r-studio
COPY ./shiny-server-1.5.2.837-amd64.deb /tmp/ss.deb
RUN gdebi -n /tmp/ss.deb && \
    rm -f /tmp/ss.deb

COPY ./shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

# now to the R part...


# first we need devtools for all the installation of all further packages
RUN R -e 'install.packages("devtools", repos = "http://cloud.r-project.org/")'

# install all the packages we need from cran, bioconductor and github

RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite()'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("BiocParallel")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("rtracklayer")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("RamiGO")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("BiocGenerics")'
RUN R -e 'source("http://bioconductor.org/biocLite.R");biocLite("limma")'
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
RUN R -e 'devtools::install_version("dplyr", version = "0.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_github("jyyu/ScreenBEAM", ref = "d6204b3")'
RUN R -e 'devtools::install_version("readr", version = "1.0.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinydashboard", version = "0.5.3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("jsonlite", version = "1.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyBS", version = "0.61", repos = "http://cloud.r-project.org/")'
#RUN R -e 'devtools::install_version("highcharter", version = "0.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_github("jbkunst/highcharter", ref = "cc5f1e0157c50fc67233bc1bad6c3ad906d418c8")'
RUN R -e 'devtools::install_version("seqinr", version = "3.3-6", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("openxlsx", version = "4.0.0", repos = "http://cloud.r-project.org/")'
#RUN R -e 'devtools::install_github("awalker89/openxlsx", ref = "6d0fed9")'
RUN R -e 'devtools::install_version("caTools", version = "1.17.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("reshape2", version = "1.4.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("MESS", version = "0.4-3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("igraph", version = "1.0.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("data.table", version = "1.10.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_github("jimhester/gmailr", ref = "d5ee3c2")'
RUN R -e 'devtools::install_version("d3heatmap", version = "0.6.1.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("DEoptim", version = "2.2-3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("httr", version = "1.2.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("sgRSEA", version = "0.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("plyr", version = "1.8.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("VennDiagram", version = "1.6.17", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("tidyverse", version = "1.0.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("htmltools", version = "0.3.5", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("DT", version = "0.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("tidyr", version = "0.6.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("sm", version = "2.2-5.4", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shinyjs", version = "0.9", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("ggplot2", version = "2.2.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("markdown", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("knitr", version = "1.15.1", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("shiny", version = "1.0.2", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("bookdown", version = "0.3", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("R.utils", version = "2.5.0", repos = "http://cloud.r-project.org/")'
RUN R -e 'devtools::install_version("htmltools", version = "0.3.5", repos = "http://cloud.r-project.org/")'



# cleaning up downloaded deb packages for keeping clean our docker image
RUN apt-get -qq clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# now install the reannotate-crispr PERL package
# for CPAN to auto say yes to every question
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
RUN cp -r /tmp/Supplemental-Material/crispr-reannotation /opt/
RUN chmod +x /opt/crispr-reannotation/reannotate_crispr.pl
env PATH /opt/crispr-reannotation:$PATH
RUN rm -rf /tmp/Supplemental-Material
# install intervaltree...another dependency for the crispr reannotator
RUN cd /opt/crispr-reannotation/depends/Set-IntervalTree-0.10-OD; perl Makefile.PL; make; make test && make install

# Install Python NumPy and SciPy required for BAGEL
RUN apt-get update && apt-get install -y python-numpy python-scipy python-sympy

# install software package bowtie 2 which has a difficult license and therefore cannot be packaged or shipped in a docker image
# TODO: check if this can be used in open source packages at all
COPY ./bowtie2-2.2.9-linux-x86_64.zip /tmp/
RUN unzip /tmp/bowtie2-2.2.9-linux-x86_64.zip -d /opt
#ENV PATH /opt/bowtie2-2.2.9:$PATH
RUN echo 'export PATH=/opt/bowtie2-2.2.9/:$PATH' >> /etc/profile

# install bowtie
COPY ./bowtie-1.2-linux-legacy-x86_64.zip /tmp/
RUN unzip /tmp/bowtie-1.2-linux-legacy-x86_64.zip -d /opt
RUN echo 'export PATH=/opt/bowtie-1.2-legacy/:$PATH' >> /etc/profile


# install MAGeCK from Sourceforge
COPY ./mageck-0.5.5.tar.gz /tmp/
RUN cd /tmp/; tar xvf ./mageck-0.5.5.tar.gz; cd /tmp/mageck-0.5.5; python setup.py install
RUN rm -rf /tmp/mageck-0.5.5 /tmp/mageck-0.5.5.tar.gz

# Cloning CRISPRAnalyzeR
RUN git clone git@github.com:boutroslab/CRISPRAnalyzeR.git /tmp/

# Copy to the shiny-server
COPY /tmp/CRISPRAnalyzeR-master/source/ /srv/shiny-server/CRISPRAnalyzeR

# we will run crispranalyzer as user 
RUN chown -R shiny:shiny /srv/shiny-server/CRISPRAnalyzeR



# add R profile options

RUN echo 'setwd("/srv/shiny-server/CRISPRAnalyzeR")' >> /usr/local/lib/R/etc/Rprofile.site
RUN echo 'options(download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site


# BEGIN RUST CODE
# install  RUST compiler with mapper and extractor components
RUN \
    curl https://sh.rustup.rs > /tmp/sh.rustup.rs && \
    chmod +x /tmp/sh.rustup.rs && \
    /tmp/sh.rustup.rs -y && \
rm /tmp/sh.rustup.rs

ENV PATH=/root/.cargo/bin:$PATH

# compile CRISPRAnalyzer mapper and extractor
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

##### end of RUST CODE




COPY docker-entrypoint.sh /
COPY index.html /srv/shiny-server
RUN chmod +x /docker-entrypoint.sh
RUN chmod +x /srv/shiny-server/index.html

EXPOSE 3838

# Add ENV for KiteMatic
ENV websockets_behind_proxy=FALSE
ENV verbose_logfiles=TRUE
ENV COSMIC_database="CosmicMutantExport.tsv"
ENV EnrichR_URL="http://amp.pharm.mssm.edu/Enrichr/
ENV EnrichR=TRUE
ENV bowtie_threads=2
ENV proxy_url=
ENV proxy_port=
ENV max_upload=4096

ENTRYPOINT ["/docker-entrypoint.sh"]
# finally run
CMD ["crispranalyzer"]
