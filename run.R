system("rsync -avz --delete ~/sovellukset/tellinkiappi/ /srv/shiny-server/tellinkiappi_old/")
# system('rsync -avzhe "ssh -i /home/aurelius/avaimet/amazon_ec218.pem" --delete --progress --exclude=.Rproj.user/ --exclude=.git ~/sovellukset/tellinkiappi/ ubuntu@34.245.236.51:/srv/shiny-server/tellinkiappi/')
