FROM bigtruedata/scala:alpine

COPY . /opt/org-structure-cli/

WORKDIR /opt/org-structure-cli

ENTRYPOINT [ "./org.scala" ]
