###
# Image pour la compilation
FROM maven:3-eclipse-temurin-11 as build-image
WORKDIR /build/
# Installation et configuration de la locale FR
RUN apt update && DEBIAN_FRONTEND=noninteractive

# On lance la compilation Java
# On débute par une mise en cache docker des dépendances Java
# cf https://www.baeldung.com/ops/docker-cache-maven-dependencies
COPY ./pom.xml /build/pom.xml
COPY ./core/pom.xml /build/core/pom.xml
COPY ./web/pom.xml /build/web/pom.xml
COPY ./batch/pom.xml /build/batch/pom.xml
RUN mvn verify --fail-never
# et la compilation du code Java
COPY ./core/   /build/core/
COPY ./web/    /build/web/
COPY ./batch/  /build/batch/
RUN mvn --batch-mode \
        -Dmaven.test.skip=false \
        -Duser.timezone=Europe/Paris \
        -Duser.language=fr \
        package
###
# Image pour le module API
#FROM tomcat:9-jdk11 as api-image
#COPY --from=build-image /build/web/target/*.war /usr/local/tomcat/webapps/ROOT.war
#CMD [ "catalina.sh", "run" ]
FROM eclipse-temurin:11-jre as api-image
WORKDIR /app/
COPY --from=build-image /build/web/target/*.jar /app/periscope.jar
ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
ENTRYPOINT ["java", "-XX:MaxRAMPercentage=95","-jar","/app/periscope.jar"]


###
# Image pour le module batch
FROM rockylinux:8 as batch-image
WORKDIR /scripts/

# Installer les paquets nécessaires
RUN dnf install -y java-11-openjdk cronie procps gettext

COPY --from=build-image /build/batch/target/*.jar /scripts/periscope-batch.jar
RUN chmod +x /scripts/periscope-batch.jar

COPY ./docker/run_batch.sh /scripts/run_batch.sh
RUN sed -i 's/\r$//' /scripts/run_batch.sh
RUN chmod +x /scripts/run_batch.sh
RUN touch /var/log/cron.log
COPY ./docker/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

CMD tail -f /var/log/cron.log
