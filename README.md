#Shiny server config
sudo nano /etc/shiny-server/shiny-server.conf
#Restart app
sudo /sbin/service shiny-server restart
#Yes
Hash value:1vf3c981vf3c981vf3c98
#Shiny Log dir
log_dir /var/log/shiny-server;
#Must install all packages listed in app.R 
SERVER: sudo su - -c "R -e \"install.packages('tidyverse')\""
CLIENT : R install.packages('tidyverse')