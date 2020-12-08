# Shiny server config (everything is currently being logged)
sudo nano /etc/shiny-server/shiny-server.conf
# Restart app
sudo /sbin/service shiny-server restart
# Shiny Log dir
log_dir /var/log/shiny-server;
# Must install all packages listed in app.R 
SERVER: sudo su - -c "R -e \"install.packages('tidyverse')\""
CLIENT : R install.packages('tidyverse')
