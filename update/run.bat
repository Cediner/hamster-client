mkdir logs
java -Xmx1024m -jar hafen-updater.jar update https://gitlab.com/Boshaw/hamster-client/raw/master/update/ -Djava.util.logging.config.file=logging.properties -Xmx1024M -jar hafen.jar