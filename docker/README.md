# How to build the docker image for CRISPRAnalyzeR

Download a source-code release version and extract it into a new folder.  
Open the folder and navigate to the **docker** folder, where the **dockerfile** is located. This will be the folder that is used to create the docker image file.

e.g. `/userpath/extracted/CRISPRAnalyzer/docker/`  

Everything mentioned below will take place there!  

In general, CRISPRAnalyzeR source code will be retrieved by cloning the GitHub repository. In principle, this can be changed to use any fork of CRISPRAnalzyeR, as long as the file structure remains the same!

## Start Docker

Start docker and make sure you have set a proxy, if required. 

## Make sure you have internet access configured

While building CRISPRAnalyzeR, the tool will download all required files, so please make sure internet access (e.g. proxy) has been set correctly.


## Build the image

Finally build the image, which takes 1-3 hours depending on the computer and network speed.

```bash
docker build -t CRISPRAnalyzeR .
```


## Test the image

```bash
docker run --rm -p 8000:8000 CRISPRAnalyzeR
```

Check it by opening a browser tab and navigating to 

```
http://localhost:8000/CRISPRAnalyzeR
```

If this works you have successfully created a CRISPRAnalyzeR docker image!

