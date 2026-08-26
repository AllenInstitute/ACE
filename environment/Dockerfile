# hash:sha256:589de819f5b383a3c66f4452a9776bbd56a46703481c101e3fd45b1e7d25aefc
ARG REGISTRY_HOST
FROM $REGISTRY_HOST/codeocean/py-r:python3.9-R4.2.2-IRkernel-ubuntu20.04

ARG DEBIAN_FRONTEND=noninteractive
ARG GIT_ASKPASS
COPY git-ask-pass /

ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_PATHS_CACHE=/opt/renv/cache
ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org
ENV R_LIBS_USER=/opt/renv/library

COPY postInstall /
RUN --mount=type=secret,id=secrets . /run/secrets/secrets \
    && /postInstall
