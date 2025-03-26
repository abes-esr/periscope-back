###
# Image pour la compilation
FROM maven:3-eclipse-temurin-11 as build-image
WORKDIR /build/
# Installation et configuration de la locale FR
RUN apt update && DEBIAN_FRONTEND=noninteractive apt -y install locales
RUN sed -i '/fr_FR.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
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
ENV LANG fr_FR.UTF-8
ENV LANGUAGE fr_FR:fr
ENV LC_ALL fr_FR.UTF-8
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
ENTRYPOINT ["java", "-XX:MaxRAMPercentage=95","-jar","/app/periscope.jar"]


###
# Image pour le module batch
FROM rockylinux:8 as batch-image
WORKDIR /scripts/
ENV TZ=Europe/Paris
ENV LANG fr_FR.UTF-8
ENV LANGUAGE fr_FR:fr
ENV LC_ALL fr_FR.UTF-8

# Le JAR et le script pour le batch de LN
RUN dnf install -y java-11-openjdk
RUN dnf install -y at  # Correction : utilisation de dnf
RUN dnf install -y glibc-langpack-fr #ajout des langue française.
RUN localedef -i fr_FR -f UTF-8 fr_FR.UTF-8 # creation de la locale.
COPY --from=build-image /build/batch/target/*.jar /scripts/periscope-batch.jar
RUN chmod +x /scripts/periscope-batch.jar

COPY ./docker/run_batch.sh /scripts/run_batch.sh
RUN chmod +x /scripts/run_batch.sh

CMD ["tail", "-f", "/dev/null"]
