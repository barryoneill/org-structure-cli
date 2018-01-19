FROM bigtruedata/scala:alpine

COPY . /opt/org-structure-cli/

WORKDIR /opt/org-structure-cli

RUN ./org 2>/dev/null || true

ENTRYPOINT [ "./org" ]
