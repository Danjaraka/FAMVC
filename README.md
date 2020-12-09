# Can be found in action here http://110.173.229.205/Dashboard/
# Shiny server config (everything is currently being logged)
sudo nano /etc/shiny-server/shiny-server.conf
# Restart app
sudo systemctl restart shiny-server
# Must install all packages listed in app.R 
SERVER: sudo su - -c "R -e \"install.packages('tidyverse')\""
CLIENT : R install.packages('tidyverse')