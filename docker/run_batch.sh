#!/bin/bash
java -Djava.security.egd=file:///dev/urandom -jar periscope-batch.jar --spring.batch.job.names=indexerTableNoticesBibio
