#!/bin/bash

set -e

CRISPR_CFG=/srv/shiny-server/CRISPRAnalyzeR/config.r

# subs definition go here

# usage: key_value_to_cfg key value
# to the crispranalyzer config file
# example: key_value_to_cfg "car.proxy" "'value'" "/srv/shiny-server/CRISPRAnalyzeR/config.r" 
key_value_to_cfg() {
        local key="$1"
        local value="$2"
        sed -i  's#^config\[\["'$key'"\]\] <-.*$#config\[\["'$key'"\]\] <- '$value'#g' "$3"
}

# end of subs definitions

# if we are a crispranalyzer
if [ "$1" = 'crispranalyzer' ]; then
  if [ "$websockets_behind_proxy" ]; then
     echo "[CUSTOM] setting enhanced websocket settings"
     # make changes to the app config, this is needed for some problematic
     # cooperate proxy servers
     sed -i.bak.proxy 's#location / {#location / {\napp_init_timeout 18000;\napp_idle_timeout 18000;\ndisable_protocols  xhr-streaming xhr-polling xdr-polling iframe-xhr-polling jsonp-polling;\n#g' /etc/shiny-server/shiny-server.conf 
  fi  
  if [ "$verbose_logfiles" ]; then
     echo "[CUSTOM] setting verbose shiny server and application logfiles"
     # this is a very useful setting for debugging your shiny application..this keeps all logfiles in /var/log/shiny-server. 
     # otherwise log files get deleted if the app crashes[CUSTOM] which is usually not what we want when debugging
     # but dont use this option on production server[CUSTOM] this will fill up space easily
     sed -i.bak.verbose 's#run_as shiny;#run_as shiny;\npreserve_logs true; #g' /etc/shiny-server/shiny-server.conf
     # enable full debugging output for the shiny server
     sed -i.bak.verbose 's#exec shiny-server\(.*\)#export SHINY_LOG_LEVEL=TRACE\nexec shiny-server \1#g' /usr/bin/shiny-server.sh
	 key_value_to_cfg "downloadlogs" "\"$verbose_logfiles\"" "$CRISPR_CFG"
  fi 
  # default is "./database", overwrite if environment variable is set  
  #if [ "$database_path" ]; then
  #  echo "[CUSTOM] changing the database path"
  #  key_value_to_cfg "database_path" "\"$database_path\"" "$CRISPR_CFG" 
  #fi
  if [ "$COSMIC_database" ]; then
    echo "[CUSTOM] setting the path to the COSMIC database"  
    key_value_to_cfg "COSMIC_database" "\"$COSMIC_database\"" "$CRISPR_CFG"
  fi
  if [ "$EnrichR" ]; then
    echo "[CUSTOM] EnrichR interface"
    key_value_to_cfg "EnrichR" "\"$EnrichR\"" "$CRISPR_CFG"
  fi
  if [ "$EnrichR_URL" ]; then
    echo "[CUSTOM] setting the EnrichR URL"
    key_value_to_cfg "EnrichR_URL" "\"$EnrichR_URL\"" "$CRISPR_CFG"
  fi
  # if [ "$ecrisp_databasepath" ]; then
#     echo "[CUSTOM] setting E-CRISP Re-annotation database path"
#     key_value_to_cfg "databasepath" "\"$ecrisp_databasepath\"" "$CRISPR_CFG"
#   fi
	if [ "$max_upload" ]; then
    echo "[CUSTOM] Set maximum upload Size"
    key_value_to_cfg "max_upload" "\"$max_upload\"" "$CRISPR_CFG"
  fi
  if [ "$bowtie_threads" ]; then
    echo "[CUSTOM] setting number of threads for Bowtie2"
    key_value_to_cfg "car.bt2.threads" "$bowtie_threads" "$CRISPR_CFG"
  fi
  if [ "$proxy_url" ]; then
    echo "[CUSTOM] setting PROXY URL"
    key_value_to_cfg "car.proxy.url" "\"$proxy_url\"" "$CRISPR_CFG"
  fi
  if [ "$proxy_port" ]; then
    echo "[CUSTOM] setting PROXY PORT"
    key_value_to_cfg "car.proxy.port" "$proxy_port" "$CRISPR_CFG"
  fi
  if [ "$downloadlogs" ]; then
    echo "[CUSTOM] setting Logs"
    key_value_to_cfg "downloadlogs" "$downloadlogs" "$CRISPR_CFG"
  fi
  # Copy config.r to all copies
  cp /srv/shiny-server/CRISPRAnalyzeR/config.r /srv/shiny-server/CRISPRAnalyzeR2/config.r
  cp /srv/shiny-server/CRISPRAnalyzeR/config.r /srv/shiny-server/CRISPRAnalyzeR3/config.r
  cp /srv/shiny-server/CRISPRAnalyzeR/config.r /srv/shiny-server/CRISPRAnalyzeR4/config.r
  cp /srv/shiny-server/CRISPRAnalyzeR/config.r /srv/shiny-server/CRISPRAnalyzeR5/config.r
  
  echo "Starting Shiny-Server"
  echo "As a default, please navigate to http://localhost:8000/CRISPRAnalyzeR/ to access CRISPRAnalyzeR"
  exec /usr/bin/shiny-server.sh
  echo "Shiny-Server started"
fi
