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
    shadow-cljs compile dhcp-client dhcp-server ping-client read-pcap test && \
    chmod +x build/*.js


FROM node:16-slim as run

RUN apt-get -y update
RUN apt-get -y install libpcap-dev tcpdump iputils-ping curl

COPY --from=build /app/ /app/

