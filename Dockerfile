FROM node:16 as build

RUN apt-get -y update && \
    apt-get -y install libpcap-dev default-jdk-headless

RUN npm install -g shadow-cljs

# Separate npm and clojure deps from main app build
RUN mkdir -p /app
ADD shadow-cljs.edn package.json /app/
RUN cd /app && npm --unsafe-perm install
RUN cd /app && shadow-cljs info

# main app build
ADD src/ /app/src/
ADD test/ /app/test/
RUN cd /app && \
    shadow-cljs compile simple-client pool-server mac2ip-server ping-client read-pcap test && \
    chmod +x build/*.js

FROM build as dev

RUN apt-get -y install tcpdump
RUN apt-get -y install isc-dhcp-client isc-dhcp-server iputils-ping curl iproute2
RUN curl -L https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o /usr/local/bin/lein && \
    chmod +x /usr/local/bin/lein

FROM node:16-slim as run

RUN apt-get -y update
RUN apt-get -y install libpcap-dev tcpdump iputils-ping curl iproute2

ENTRYPOINT []

COPY --from=build /app/ /app/

