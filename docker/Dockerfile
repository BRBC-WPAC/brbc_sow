FROM rocker/verse:4

ENV TZ=America/Edmonton
ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /work
COPY . .

RUN apt-get update -qq
RUN apt-get install --no-install-recommends -y \
    gpg \
    apt-utils \
    ca-certificates \
    gdebi-core \
    curl

RUN if [ ! -f code.deb ]; then curl -sL "https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64" -o code.deb; fi
RUN gdebi -n code.deb

RUN echo "copilot-enabled=1" >> /etc/rstudio/rsession.conf
RUN Rscript requirements.R