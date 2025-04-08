#!/bin/bash

env
echo "lancement du batch d'indexation"
java -Djava.security.egd=file:///dev/urandom -jar /scripts/periscope-batch.jar --spring.batch.job.names=indexerTableNoticesBibio
