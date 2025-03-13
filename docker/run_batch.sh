unset SPRING_PROFILES_ACTIVE
java -Djava.security.egd=file:///dev/urandom -jar periscope-batch-1.0.0-SNAPSHOT.jar --spring.batch.job.names=indexerTableNoticesBibio --spring.profiles.active=prod
