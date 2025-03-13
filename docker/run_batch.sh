#!/bin/bash
if [ -z "$1" ]; then
  echo "Erreur : Veuillez fournir un profil Spring en argument."
  exit 1
fi
unset SPRING_PROFILES_ACTIVE
java -Djava.security.egd=file:///dev/urandom -jar periscope.jar --spring.batch.job.names=indexerTableNoticesBibio --spring.profiles.active=$1
