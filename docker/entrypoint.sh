#! /usr/bin/env bash

set -e
set -x

# copy the Hydat database to the rstudio user's home directory
mkdir -p /home/rstudio/.local/share/tidyhydat
cp /work/Hydat.sqlite3 /home/rstudio/.local/share/tidyhydat/
chown -R rstudio:rstudio /home/rstudio/.local/share/tidyhydat

export DONT_PROMPT_WSL_INSTALL=1
sudo -u rstudio code serve-web --port 28080 --accept-server-license-terms --host 0.0.0.0 --without-connection-token 2>&1 | tee -a /tmp/code-server.log &
sudo -u rstudio /home/rstudio/workspace/BRBC_SOW/vscode.sh # install extensions

/usr/lib/rstudio-server/bin/rserver --server-daemonize 0 --auth-none 1
