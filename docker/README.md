# How to build the docker image for CRISPRAnalyzeR

Download a source-code release version and extract it into a new folder.  
Open the folder and navigate to the **docker** folder, where the **dockerfile** is located. This will be the folder that is used to create the docker image file.

e.g. `/userpath/extracted/CRISPRAnalyzer/docker/`  

Everything mentioned below will take place there!  

In general, CRISPRAnalyzeR source code will be retrieved by cloning. In principle, this can be changed to use any fork of CRISPRAnalzyeR, as long as the file structure remains the same!

## Start Docker

Start docker and make sure you have set a proxy, if required. 

## Download the required dependencies 

First download the following dependencies to the directory containing this dockerfile

```bash
wget https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-1.5.2.837-amd64.deb
wget http://downloads.sourceforge.net/project/bowtie-bio/bowtie2/2.2.9/bowtie2-2.2.9-linux-x86_64.zip
wget https://sourceforge.net/projects/bowtie-bio/files/bowtie/1.2.0/bowtie-1.2-linux-x86_64.zip
wget http://downloads.sourceforge.net/project/mageck/0.5/mageck-0.5.5.tar.gz
```

## Add the CRISPRAnalyzeR source code

Make a new folder **caR-release**

```bash
mkdir ./caR-release
```

Now you can just copy the contents of the **source** folder into the **caR-release** folder OR you can clone the source from the GitHub repository.  

### Alternative 1: Copy the contents from the source folder


Just copy all files and folders from within the source folder to the caR-release folder and you can proceed **with building the image**.


### Alternative 2: Clone from GitHub repository
If you want to clone it instead of copy the source folder, clone the CRISPRAnalyzeR project into the caR-release-git folder and sync the source to caR-release. 
The content of caR-release is the source code of CRISPRAnalyzeR and will be used for building the docker image.

```bash
git clone git@github.com:boutroslab/CRISPRAnalyzeR.git ./caR-release-git

rsync -rav caR-release-git/ caR-release/source --exclude=.git
```

## Build the image

Finally build the image, which takes 1-3 hours depending on the computer and network speed.
```bash
docker build -t CRISPRAnalyzeR .
```
or squash it

```bash
docker build -squash -t CRISPRAnalyzeR .
```

## Test the image

```bash
docker run --rm -p 80:3838 CRISPRAnalyzeR
```

Check it by opening a browser tab and navigating to 

```
http://localhost/CRISPRAnalyzeR
```

If this works you have successfully created a CRISPRAnalyzeR docker image!

