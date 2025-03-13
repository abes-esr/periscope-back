#!/bin/bash
java -Djava.security.egd=file:///dev/urandom -jar periscope.jar --spring.batch.job.names=indexerTableNoticesBibio
