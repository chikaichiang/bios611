# BIOS611 Final Project

## Investigating the Causes of Cervical Cancer

Hi, this is my 611 Data Science Project. More to come.

I am interested in conducting exploratary analysis on two datasets related to cervical cancer. 
Both are from UCI Machine Learning Database. The first dataset called kag_risk_factors_cervical_cancer.csv which describes the common well-known risk factors related to cervical cancer. There are four cervical cancer medical test related response variables, Hinselmann, Citoloogy, Biopsy and Schiller. 

The second dataset called cervical_cancer_behave_risk.csv which the risk factors related to the study on behavioral science. The only cervical cancer response variable is binary. Check the data desciption more in detail from my report.  

The original datasets are here: 

https://www.archive.ics.uci.edu/dataset/383/cervical+cancer+risk+factors

https://archive.ics.uci.edu/dataset/537/cervical+cancer+behavior+risk


## Instruction to build the docker file:

### 1: Clone the repository

Clone the repository in git bash using the repository URL.

```
git clone https://github.com/chikaichiang/bios611_qikai.git
```

### 2: Build the docker image

In a new terminal/window, set the current working directory to the cloned folder.

```
cd bios611
```
Build the docker container using the docker build command with image name. For example, to build an image named 611 use the following command:


```
docker build . -t bios611
```

### 3: Run the docker container

For Mac/terminal, use the following command to run the container.

```
docker run "docker run -v $(pwd):/home/rstudio/work -p 8787:8787 -it bios611"
```

For Powershell, use the following command to run the container.

```
docker run "docker run -v ${pwd}:/home/rstudio/work -p 8787:8787 -it bios611"
```

Replace 611 with the image name used to build the container 


#### 4. Running Rstudio Docker container

Once the container is built and run, open the rstudio container image in a web browser. 

Type http://localhost:8787  in a browser window. 
Use "rstudio" as username and password generated in the terminal from previous step.
This Rstudio image should have the necessary libraries and dependencies required for this project.

Once you are into the rstudio docker container set the 'work' directory as working directory using terminal interface. 

```
cd work
ls
```

`ls` command should list all the files in the repository, including a Dockerfile and Makefile.


#### 5. Makefile

The Makefile is used to create the report and images. Please see the makefile for information about the relationship between the scripts and outputs. 
The outputs will be created in the "work" folder. Please check the files in work folder for output. 


  To create the final report run "make report.html". This will create "Report.html" in the work folder. 

```
make report.html
```
  
  To create a figure describing the demographics run "make age_bar.png"
	
```
make age_bar.png
```

  To run all the scripts in the Makefile
  
```
make all
```
