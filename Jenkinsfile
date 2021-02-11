//this is the scripted method with groovy engine
import hudson.model.Result

node {

    //Configuration
    def gitURL = "https://github.com/abes-esr/periscope-back.git"
    def gitCredentials = ''
    def warDir = "web/target/"
    def warName = "periscope"
    def tomcatWebappsDir = "/usr/local/tomcat9-periscope/webapps/"
    def tomcatServiceName = "tomcat9-periscope.service"
    def slackChannel = "#notif-periscope"

    // Variables globales
    def maventool
    def rtMaven
    def buildInfo
    def server
    def ENV
    def serverHostnames = []
    def serverCredentials = []
    def executeTests

    // Configuration du job Jenkins
    // On garde les 5 derniers builds par branche
    // On scanne les branches et les tags du Git
    properties([
            buildDiscarder(
                    logRotator(
                            artifactDaysToKeepStr: '',
                            artifactNumToKeepStr: '',
                            daysToKeepStr: '',
                            numToKeepStr: '5')
            ),
            parameters([
                    gitParameter(
                            branch: '',
                            branchFilter: 'origin/(.*)',
                            defaultValue: 'develop',
                            description: 'Sélectionner la branche ou le tag à déployer',
                            name: 'BRANCH_TAG',
                            quickFilterEnabled: false,
                            selectedValue: 'NONE',
                            sortMode: 'DESCENDING_SMART',
                            tagFilter: '*',
                            type: 'PT_BRANCH_TAG'),
                    choice(choices: ['DEV', 'TEST', 'PROD'], description: 'Sélectionner l\'environnement cible', name: 'ENV'),
                    booleanParam(defaultValue: false, description: 'Voulez-vous exécuter les tests ?', name: 'executeTests')
            ])
    ])

    stage('Set environnement variables') {
        try {
            env.JAVA_HOME = "${tool 'Open JDK 11'}"
            env.PATH = "${env.JAVA_HOME}/bin:${env.PATH}"

            maventool = tool 'Maven 3.3.9'
            rtMaven = Artifactory.newMavenBuild()
            server = Artifactory.server '-1137809952@1458918089773'
            rtMaven.tool = 'Maven 3.3.9'
            rtMaven.opts = '-Xms1024m -Xmx4096m'

            if (params.BRANCH_TAG == null) {
                throw new Exception("Variable BRANCH_TAG is null")
            } else {
                echo "Branch to deploy =  ${params.BRANCH_TAG}"
            }

            if (params.ENV == null) {
                throw new Exception("Variable ENV is null")
            } else {
                ENV = params.ENV
                echo "Target environnement =  ${ENV}"
            }

            if (ENV == 'DEV') {
                serverHostnames.add('hostname.server1-dev')
                serverCredentials.add('cirse1-dev-ssh-key')

                serverHostnames.add('hostname.server2-dev')
                serverCredentials.add('cirse2-dev-ssh-key')

            } else if (ENV == 'TEST') {
                serverHostnames.add('hostname.server1-test')
                serverCredentials.add('cirse1-test-ssh-key')

                serverHostnames.add('hostname.server2-test')
                serverCredentials.add('cirse2-test-ssh-key')

            } else if (ENV == 'PROD') {
                serverHostnames.add('hostname.server1-prod')
                serverCredentials.add('cirse1-prod-ssh-key')

                serverHostnames.add('hostname.server2-prod')
                serverCredentials.add('cirse2-prod-ssh-key')
            }

            if (params.executeTests == null) {
                executeTests = false
            } else {
                executeTests = params.executeTests
            }

            echo "executeTests =  ${executeTests}"

        } catch (e) {
            currentBuild.result = hudson.model.Result.NOT_BUILT.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    stage('SCM checkout') {
        try {
            checkout([
                    $class                           : 'GitSCM',
                    branches                         : [[name: "${params.BRANCH_TAG}"]],
                    doGenerateSubmoduleConfigurations: false,
                    extensions                       : [],
                    submoduleCfg                     : [],
                    userRemoteConfigs                : [[credentialsId: "${gitCredentials}", url: "${gitURL}"]]
            ])

        } catch (e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    if ("${executeTests}" == 'true') {
        stage('test') {
            try {

                rtMaven.run pom: 'pom.xml', goals: 'clean test'
                junit allowEmptyResults: true, testResults: '/target/surefire-reports/*.xml'

            } catch (e) {
                currentBuild.result = hudson.model.Result.UNSTABLE.toString()
                notifySlack(slackChannel,e.getLocalizedMessage())
                // Si les tests ne passent pas, on mets le build en UNSTABLE et on continue
                //throw e
            }
        }
    } else {
        echo "Tests are skipped"
    }

    stage('compile-package') {
        try {
            sh 'cd '
            if (ENV == 'DEV') {
                echo 'compile for dev profile'
                sh "'${maventool}/bin/mvn' -Dmaven.test.skip=true clean package -DfinalName='${warName}' -DbaseDir='${tomcatWebappsDir}${warName}' -Pdev"
            }

            if (ENV == 'TEST') {
                echo 'compile for test profile'
                sh "'${maventool}/bin/mvn' -Dmaven.test.skip=true clean package -DfinalName='${warName}' -DbaseDir='${tomcatWebappsDir}${warName}' -Ptest"
            }

            if (ENV == 'PROD') {
                echo 'compile for prod profile'
                sh "'${maventool}/bin/mvn' -Dmaven.test.skip=true clean package -DfinalName='${warName}' -DbaseDir='${tomcatWebappsDir}${warName}' -Pprod"
            }

        } catch(e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    //stage('sonarqube analysis'){
    //   withSonarQubeEnv('SonarQube Server2'){ cf : jenkins/configuration/sonarQube servers ==> between the quotes put the name we gave to the server
    //      sh "${maventool}/bin/mvn sonar:sonar"
    //  }
    // }

    stage('artifact') {
        try {
            archive "${warDir}${warName}.war"

        } catch(e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    stage ('stop tomcat'){
        for (int i = 0; i < serverHostnames.size(); i++) { //Pour chaque serveur
            try {
                echo "stop tomcat on ${serverHostnames[i]}"

                sshagent(credentials: ["${serverCredentials[i]}"]) {
                    withCredentials([
                            usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username'),
                            string(credentialsId: "${serverHostnames[i]}", variable: 'hostname'),
                            string(credentialsId: 'service.status', variable: 'status'),
                            string(credentialsId: 'service.stop', variable: 'stop'),
                            string(credentialsId: 'service.start', variable: 'start')
                    ]) {
                        try {
                            echo 'get service status'
                            sh "ssh -tt ${username}@${hostname} \"${status} ${tomcatServiceName}\""

                            echo 'stop the service'
                            sh "ssh -tt ${username}@${hostname} \"${stop} ${tomcatServiceName}\""

                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'maybe the service is not running'

                            echo 'we try to start the service'
                            sh "ssh -tt ${username}@${hostname} \"${start} ${tomcatServiceName}\""

                            echo 'get service status'
                            sh "ssh -tt ${username}@${hostname} \"${status} ${tomcatServiceName}\""

                            echo 'stop the service'
                            sh "ssh -tt ${username}@${hostname} \"${stop} ${tomcatServiceName}\""
                        }
                    }
                }
            } catch(e) {
                currentBuild.result = hudson.model.Result.FAILURE.toString()
                notifySlack(slackChannel,e.getLocalizedMessage())
                throw e
            }
        }
    }

    stage ('deploy to tomcat'){
        for (int i = 0; i < serverHostnames.size(); i++) { //Pour chaque serveur
            try {
                echo "deploy to tomcat on ${serverHostnames[i]}"

                sshagent(credentials: ["${serverCredentials[i]}"]) {
                    withCredentials([
                            usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username'),
                            string(credentialsId: "${serverHostnames[i]}", variable: 'hostname')
                    ]) {
                        sh "ssh -tt ${username}@${hostname} \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                        sh "scp ${warDir}${warName}.war ${username}@${hostname}:${tomcatWebappsDir}"
                    }
                }
            } catch(e) {
                currentBuild.result = hudson.model.Result.FAILURE.toString()
                notifySlack(slackChannel,e.getLocalizedMessage())
                throw e
            }
        }
    }

    stage ('restart tomcat'){
        for (int i = 0; i < serverHostnames.size(); i++) { //Pour chaque serveur
            try {
                echo "restart tomcat on ${serverHostnames[i]}"

                sshagent(credentials: ["${serverCredentials[i]}"]) {
                    withCredentials([
                            usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username'),
                            string(credentialsId: "${serverHostnames[i]}", variable: 'hostname'),
                            string(credentialsId: 'service.status', variable: 'status'),
                            string(credentialsId: 'service.start', variable: 'start')
                    ]) {
                        echo 'start service'
                        sh "ssh -tt ${username}@${hostname} \"${start} ${tomcatServiceName}\""

                        echo 'get service status'
                        sh "ssh -tt ${username}@${hostname} \"${status} ${tomcatServiceName}\""
                    }
                }
            } catch(e) {
                currentBuild.result = hudson.model.Result.FAILURE.toString()
                notifySlack(slackChannel,e.getLocalizedMessage())
                throw e
            }
        }
    }

    stage ('Artifactory configuration') {
        try {
            rtMaven.deployer server: server, releaseRepo: 'libs-release-local', snapshotRepo: 'libs-snapshot-local'
            buildInfo = Artifactory.newBuildInfo()
            buildInfo = rtMaven.run pom: 'pom.xml', goals: '-U clean install -Dmaven.test.skip=true '

            rtMaven.deployer.deployArtifacts buildInfo
            buildInfo = rtMaven.run pom: 'pom.xml', goals: 'clean install -Dmaven.repo.local=.m2 -Dmaven.test.skip=true'
            buildInfo.env.capture = true
            server.publishBuildInfo buildInfo

        } catch(e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    currentBuild.result = hudson.model.Result.SUCCESS.toString()
    notifySlack(slackChannel,"Congratulation !")
}

def notifySlack(String slackChannel, String info = '') {
    def colorCode = '#848484' // Gray

    switch (currentBuild.result) {
        case 'NOT_BUILT':
            colorCode = '#FFA500' // Orange
            break
        case 'SUCCESS':
            colorCode = '#00FF00' // Green
            break
        case 'UNSTABLE':
            colorCode = '#FFFF00' // Yellow
            break
        case 'FAILURE':
            colorCode = '#FF0000' // Red
            break;
    }

    String message = """
        *Jenkins Build*
        Job name: `${env.JOB_NAME}`
        Build number: `#${env.BUILD_NUMBER}`
        Build status: `${currentBuild.result}`
        Branch or tag: `${params.BRANCH_TAG}`
        Target environment: `${params.ENV}`
        Message: `${info}`
        Build details: <${env.BUILD_URL}/console|See in web console>
    """.stripIndent()

    return slackSend(tokenCredentialId: "slack_token",
            channel: "${slackChannel}",
            color: colorCode,
            message: message)
}
