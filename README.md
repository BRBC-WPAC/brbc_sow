# BRBC SOW

## Project overview

Data compilation and Analytics project for the [Bow River Basin Council (BRBC)](https://brbc.ab.ca/) *State of the Watershed* report for 2023.

### Code

The code in this project is organized such that it should *just run* if you have [Docker](https://www.docker.com/) installed.
Refer to Docker's [installation guidance](https://www.docker.com/get-started/) for getting it set up on your workstation.

### Development environments

Once the project is set up and running (see below for instructions), locally-accessible instances of RStudio and Visual Studio Code will be running.

- [RStudio at localhost:18787](http://localhost:18787)
- [Visual Studio Code at localhost:25005](http://localhost:25005/?folder=/home/rstudio/workspace/BRBC_SOW)

## Building and running the docker container

Before you can access the development environments, the docker container must be built and started.
The build process will install all of the necessary R packages needed to run the plot-generation scripts.
It also downloads the HYDAT file, which can take some time.

### Using a graphical docker application

Your installation of docker may provide a GUI for starting up a container.
If you will be using a GUI tool, please refer to its documentation for building and running the container.
The container definition is in the `docker-compose.yaml` file.

### Using the bash command-line

From the bash command-line, you can build and run the container with the following command.

```bash
docker compose up -d
```

## Generating the plots

Run `run.sh` from the CLI to generate the plots.
Once complete, generated images can be found in the `output/figures` folder inside the container.
From your host workstation, is folder can be found in the `R/output/figures` subfolder of the folder where you cloned this repository into.
Furthermore, the images generated will be within subfolders of the `output/figures` folder, organized by the type of plot (individual or faceted).

```bash
cd /home/rstudio/workspace/BRBC_SOW
./run.sh
```


