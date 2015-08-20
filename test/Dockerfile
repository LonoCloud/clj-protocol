FROM ciena/mininet

RUN apt-get update
RUN apt-get install -y python-netaddr python-daemon isc-dhcp-client

RUN cd / && curl -O https://nodejs.org/dist/v0.12.7/node-v0.12.7-linux-x64.tar.gz && \
    tar xzf node-v0.12.7-linux-x64.tar.gz && \
    ln -sf /node-v0.12.7-linux-x64/bin/node /usr/bin/node && \
    rm node-v0.12.7-linux-x64.tar.gz

# SSH config
RUN mkdir -p /var/run/sshd
RUN ssh-keygen -N '' -f /root/.ssh/id_rsa
RUN cat /root/.ssh/id_rsa.pub >> /root/.ssh/authorized_keys
