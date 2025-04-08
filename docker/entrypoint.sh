#!/bin/bash

# Réglage de /etc/environment pour que les crontab s'exécutent avec les bonnes variables d'env
echo "$(env)
LANG=fr_FR.UTF-8" > /etc/environment

echo "00 22 * * * /scripts/run_batch.sh >> /var/log/cron.log 2>&1 && crontab -r" > /etc/cron.d/one-time-job
echo "-> Installation des crontab :"
cat /etc/cron.d/one-time-job
crontab /etc/cron.d/one-time-job

exec "$@"
