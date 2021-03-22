java -Djava.util.logging.config.file=updater-logging.properties -Xmx1024m -Dhttp.agent="curl/7.51.0" -jar hamster-client-updater-1.0-shaded.jar update https://gitlab.com/Boshaw/hamster-client/raw/master/update/
java -Djava.util.logging.config.file=logging.properties -Xmx1024M -jar hafen.jar
pause