#!/bin/bash


# Paramètres par défaut du conteneur
#export PERISCOPE_BATCH_AT_STARTUP=${PERISCOPE_BATCH_AT_STARTUP:='0'}
#
## Réglage de /etc/environment pour que les crontab s'exécutent avec les bonnes variables d'env
#echo "$(env)
#LANG=en_US.UTF-8" > /etc/environment
#
## Force le démarrage du batch au démarrage du conteneur
#if [ "$PERISCOPE_BATCH_AT_STARTUP" = "1" ]; then
#  echo "-> Lancement de run_batch.sh au démarrage du conteneur"
#  ./run_batch.sh
#fi
#
#exec "$@"
