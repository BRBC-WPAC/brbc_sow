FROM rocker/verse:4

ENV TZ=America/Edmonton

WORKDIR /work
COPY . .

RUN DEBIAN_FRONTEND=noninteractive DEBCONF_NOWARNINGS=yes \
    apt-get update -qq
RUN DEBIAN_FRONTEND=noninteractive DEBCONF_NOWARNINGS=yes \
    apt-get install --no-install-recommends -y \
    gpg \
    apt-utils \
    ca-certificates \
    gdebi-core \
    curl \
    libasound2t64

RUN curl -sL "https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64" -o code.deb
RUN gdebi -n code.deb

RUN echo "copilot-enabled=1" >> /etc/rstudio/rsession.conf
RUN Rscript requirements.R