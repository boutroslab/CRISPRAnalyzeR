## README for multiuser setup
# in multiuser folder there is a ui.R, server.R and config.R
# in config.R, all paths need to be absolute and pointing to the MAIN instance of CRISPRAnalyzeR, e.g. /srv/shiny-server/CRISPRAnalyzeR
# Copy the content of multiuser as a separate increasing app number to /srv/shiny-server as configured in nginx.conf
# default:
# /srv/shiny-server/CRISPRAnalyzeR2 - 5