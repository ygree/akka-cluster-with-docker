ARG app_conf

FROM akka-cluster-demo:1.0.3

USER root

RUN apk update && \
    apk add sudo && \
    apk add curl && \
    apk add iptables && \
    # adduser -u 1000 -G wheel -D alpine && \
    rm -rf /var/cache/apk/*

# add default user with sudo rights without password

# RUN adduser -D default;                                               \
#     chgrp -R default /usr/local;                                      \
#     find /usr/local -type d | xargs chmod g+w;                        \
#     echo "default ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers.d/default; \

RUN echo "daemon ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers.d/default;  \
    chmod 0440 /etc/sudoers.d/default

# RUN mkdir /evts
# RUN chown daemon:daemon /evts
VOLUME [ "/events", "/configs", "/logs" ]

USER daemon


