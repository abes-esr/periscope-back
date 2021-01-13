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
    def executeTests
    def finalName

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
                    string(name: 'FINAL_NAME', defaultValue: "${warName}" , description: 'Nom du war/jar à déployer', ),
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

            if (params.FINAL_NAME == null || params.FINAL_NAME == '' || !(params.FINAL_NAME ==~ /^[a-zA-Z0-9_-]+/)) {
                throw new Exception("Variable FINAL_NAME is null or empty or contains special characters or whitespaces")
            } else {
                finalName = params.FINAL_NAME
                echo "Final WAR/JAR name =  ${params.FINAL_NAME}"
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
                sh "'${maventool}/bin/mvn' -Dmaven.test.skip=true clean package -DfinalName='${finalName}' -DbaseDir='${tomcatWebappsDir}' -Pdev"
            }

            if (ENV == 'TEST') {
                echo 'compile for test profile'
                sh "'${maventool}/bin/mvn' -Dmaven.test.skip=true clean package -DfinalName='${finalName}' -DbaseDir='${tomcatWebappsDir}' -Ptest"
            }

            if (ENV == 'PROD') {
                echo 'compile for prod profile'
                sh "'${maventool}/bin/mvn' -Dmaven.test.skip=true clean package -DfinalName='${finalName}' -DbaseDir='${tomcatWebappsDir}' -Pprod"
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

        try {

            if (ENV == 'DEV') {
                echo 'stop tomcat on cirse1-dev'
                sshagent(credentials: ['cirse1-dev-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        try {
                            echo 'get status cirse1 dev (should be running)'
                            sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse1 dev'
                            sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr  \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""

                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'cirse1 dev is not running'

                            echo 'we try to start cirse1 dev'
                            sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr  \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                            echo 'get status cirse1 dev'
                            sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr  \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse1 dev'
                            sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr  \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        }
                    }
                }

                echo 'stop tomcat on cirse2-dev'
                sshagent(credentials: ['cirse2-dev-ssh-key']) {

                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {
                        try {
                            echo 'get status cirse2 dev (should be running)'
                            sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse2 dev'
                            sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""

                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'cirse2 dev is not running'

                            echo 'we try to start cirse2 dev'
                            sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                            echo 'get status cirse2 dev'
                            sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse2 dev'
                            sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        }
                    }
                }
            }

            if (ENV == 'TEST') {
                echo 'stop tomcat on cirse1-test'
                sshagent(credentials: ['cirse1-test-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        try {
                            echo 'get status cirse1 test (should be running)'
                            sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse1 test'
                            sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'cirse1 test is not running'

                            echo 'we try to start cirse1 test'
                            sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                            echo 'get status cirse1 test'
                            sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse1 test'
                            sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        }
                    }
                }

                echo 'stop tomcat on cirse2-test'
                sshagent(credentials: ['cirse2-test-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        try {
                            echo 'get status cirse2 test (should be running)'
                            sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse2 test'
                            sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'cirse2 test is not running'

                            echo 'we try to start cirse2 test'
                            sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                            echo 'get status cirse2 test'
                            sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse2 test'
                            sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        }
                    }
                }
            }

            if (ENV == 'PROD') {
                echo 'stop tomcat on cirse1-prod'
                sshagent(credentials: ['cirse1-prod-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        try {
                            echo 'get status cirse1 prod (should be running)'
                            sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse1 prod'
                            sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'cirse1 prod is not running'

                            echo 'we try to start cirse1 prod'
                            sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                            echo 'get status cirse1 prod'
                            sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse1 prod'
                            sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        }
                    }
                }

                echo 'stop tomcat on cirse2-prod'
                sshagent(credentials: ['cirse2-prod-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        try {
                            echo 'get status cirse2 prod (should be running)'
                            sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse2 prod'
                            sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        } catch(e) {
                            // Maybe the tomcat is not running
                            echo 'cirse2 prod is not running'

                            echo 'we try to start cirse2 prod'
                            sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                            echo 'get status cirse2 prod'
                            sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""

                            echo 'stop cirse2 prod'
                            sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl stop ${tomcatServiceName}\""
                        }
                    }
                }
            }

        } catch(e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    stage ('deploy to tomcat'){
        try {

            if (ENV == 'DEV') {
                //here we have the choice : we can create the credential in jenkins/configuration/ssh servers
                //or in the space project (so the credential can only be accessed by the project)
                //or in jenkins/identifiants/system/identifiants globaux (so the credential can be accessed by all the projects)

                echo 'deployment on cirse1-dev'
                sshagent(credentials: ['cirse1-dev-ssh-key']) { //one key per tomcat
                    sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                    sh "scp ${warDir}${warName}.war tomcat@cirse1-dev.v3.abes.fr:${tomcatWebappsDir}"
                }

                echo 'deployment on cirse2-dev'
                sshagent(credentials: ['cirse2-dev-ssh-key']) {
                    sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                    sh "scp ${warDir}${warName}.war tomcat@cirse2-dev.v3.abes.fr:${tomcatWebappsDir}"
                }
            }
            if (ENV == 'TEST') {
                echo 'deployment on cirse1-test'
                sshagent(credentials: ['cirse1-test-ssh-key']) {
                    sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                    sh "scp ${warDir}${warName}.war tomcat@cirse1-test.v3.abes.fr:${tomcatWebappsDir}"
                }

                echo 'deployment on cirse2-test'
                sshagent(credentials: ['cirse2-test-ssh-key']) {
                    sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                    sh "scp ${warDir}${warName}.war tomcat@cirse2-test.v3.abes.fr:${tomcatWebappsDir}"
                }
            }
            if (ENV == 'PROD') {
                echo 'deployment on cirse1-prod'
                sshagent(credentials: ['cirse1-prod-ssh-key']) {
                    sh "ssh -tt tomcat@cirse1.v3.abes.fr \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                    sh "scp ${warDir}${warName}.war tomcat@cirse1.v3.abes.fr:${tomcatWebappsDir}"
                }

                echo 'deployment on cirse2-prod'
                sshagent(credentials: ['cirse2-prod-ssh-key']) {
                    sh "ssh -tt tomcat@cirse2.v3.abes.fr \"rm -r ${tomcatWebappsDir}${warName} ${tomcatWebappsDir}${warName}.war\""
                    sh "scp ${warDir}${warName}.war tomcat@cirse2.v3.abes.fr:${tomcatWebappsDir}"
                }
            }

        } catch(e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
        }
    }

    stage ('restart tomcat'){

        try {

            if (ENV == 'DEV') {
                echo 'restart tomcat on cirse1-dev'
                sshagent(credentials: ['cirse1-dev-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        echo 'start cirse1 dev'
                        sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr  \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                        echo 'finally we get status cirse1 dev (should be running)'
                        sh "ssh -tt tomcat@cirse1-dev.v3.abes.fr  \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""
                    }
                }

                echo 'restart tomcat on cirse2-dev'
                sshagent(credentials: ['cirse2-dev-ssh-key']) {

                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        echo 'start cirse2 dev'
                        sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                        echo 'finally we get status cirse2 dev (should be running)'
                        sh "ssh -tt tomcat@cirse2-dev.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""
                    }
                }
            }

            if (ENV == 'TEST') {
                echo 'restart tomcat on cirse1-test'
                sshagent(credentials: ['cirse1-test-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        echo 'start cirse1 test'
                        sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                        echo 'finally we get status cirse1 test (should be running)'
                        sh "ssh -tt tomcat@cirse1-test.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""
                    }
                }

                echo 'restart tomcat on cirse2-test'
                sshagent(credentials: ['cirse2-test-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        echo 'start cirse2 test'
                        sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                        echo 'finally we get status cirse2 test (should be running)'
                        sh "ssh -tt tomcat@cirse2-test.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""
                    }
                }
            }

            if (ENV == 'PROD') {
                echo 'restart tomcat on cirse1-prod'
                sshagent(credentials: ['cirse1-prod-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        echo 'start cirse1 prod'
                        sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                        echo 'finally we get status cirse1 prod (should be running)'
                        sh "ssh -tt tomcat@cirse1-prod.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""
                    }
                }

                echo 'restart tomcat on cirse2-prod'
                sshagent(credentials: ['cirse2-prod-ssh-key']) {
                    withCredentials([usernamePassword(credentialsId: 'tomcatuser', passwordVariable: 'pass', usernameVariable: 'username')]) {

                        echo 'start cirse2 prod'
                        sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && sudo systemctl start ${tomcatServiceName}\""

                        echo 'finally we get status cirse2 prod (should be running)'
                        sh "ssh -tt tomcat@cirse2-prod.v3.abes.fr \"cd /usr/local/ && systemctl status ${tomcatServiceName}\""
                    }
                }
            }

        } catch(e) {
            currentBuild.result = hudson.model.Result.FAILURE.toString()
            notifySlack(slackChannel,e.getLocalizedMessage())
            throw e
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
