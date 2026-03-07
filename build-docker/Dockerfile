
ARG BUILDER_IMAGE
ARG DEBIAN_TAG

FROM ${BUILDER_IMAGE} as builder

ARG BUILDER_IMAGE
ARG GHC_VERSION
ARG CABAL_VERSION

ARG CLONE_URL
ARG DNSEXT_REV
ARG PULL_REQ_NUMBER

ARG PRIVKEY_ALG
ARG PRIVKEY_ALGOPT
ARG CHAIN_SUBJ_CN

ARG GHC_OPTIMIZE
ARG GHC_PARALLEL
ARG CABAL_PARALLEL

## Install tools
RUN <<EOF
set -e -x
case ${BUILDER_IMAGE} in
  debian:*)
    ghcup_extra_pkgs='libgmp-dev libz-dev'
    setup_cmd=setup_ghcup
    ;;

  haskell:*)
    setup_cmd=:
    ;;

  *)
    echo "unknown builder image: ${BUILDER_IMAGE}"
    exit 1
    ;;
esac

setup_ghcup() {
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1                  \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1                      \
    BOOTSTRAP_HASKELL_VERBOSE=0                         \
    BOOTSTRAP_HASKELL_GHC_VERSION="${GHC_VERSION}"      \
    BOOTSTRAP_HASKELL_CABAL_VERSION="${CABAL_VERSION}"  \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1                \
    sh

  ~/.ghcup/bin/ghcup set ghc "${GHC_VERSION}"
  ~/.ghcup/bin/ghcup set cabal "${CABAL_VERSION}"
  ls -l ~/.ghcup/bin/ghc
  ls -l ~/.ghcup/bin/cabal

  echo "PATH=~/.ghcup/bin:$PATH" > ~/builder-path
}

apt-get update
DEBIAN_FRONTEND=noninteractive \
  apt-get install -y --no-install-recommends \
  build-essential binutils ca-certificates curl git openssl tini \
  ${ghcup_extra_pkgs}

$setup_cmd
EOF

WORKDIR /tmp/src

## Setup source tree
RUN <<EOF
set -e -x
git clone ${CLONE_URL}
( cd dnsext
  if [ -n "${PULL_REQ_NUMBER}" ] ; then
    git fetch origin pull/${PULL_REQ_NUMBER}/head:tmp
  fi
  git checkout ${DNSEXT_REV}
  for dir in \
    dnsext-types     \
    dnsext-svcb      \
    dnsext-dnssec    \
    dnsext-utils     \
    dnsext-do53      \
    dnsext-iterative \
    dnsext-dox       \
    dnsext-auth      \
    dnsext-bowline   \
  ; do
      touch ${dir}/CHANGELOG.md
  done )
EOF

## Configs
RUN <<EOF
set -e -x
cp dnsext/dnsext-bowline/bowline/bowline.conf bowline.conf.dist

openssl genpkey -algorithm ${PRIVKEY_ALG} -pkeyopt "${PRIVKEY_ALGOPT}" -out privkey.pem && \
openssl req -x509 -key privkey.pem -subj /CN=${CHAIN_SUBJ_CN} -out fullchain.pem && \

mkdir -p -m 755 /opt/bowline/etc
install -m 444 \
    bowline.conf.dist \
    /opt/bowline/etc/
install -m 400 privkey.pem fullchain.pem /opt/bowline/etc/
EOF

## Build and Install
## cache: in order to explicitly delete the cache, check with
## % docker buildx du --verbose --filter 'type=exec.cachemount'
## and delete with
## % docker buildx prune --verbose --filter 'id=<ID-string>'
##
RUN --mount=type=cache,target=/root/.cabal <<EOF
set -e -x
[ ! -r ~/builder-path ] || . ~/builder-path
cabal v2-update
( cd dnsext
  mkdir -p -m 755 /opt/bowline/bin
  cabal v2-install dnsext-bowline ${GHC_OPTIMIZE} \
    --enable-split-sections \
    --enable-split-objs \
    --ghc-options="-j${GHC_PARALLEL} +RTS -qg -RTS" \
    -j${CABAL_PARALLEL} --install-method=copy --installdir /opt/bowline/bin/ )
strip /opt/bowline/bin/*
EOF

RUN <<EOF
set -e -x
[ ! -r ~/builder-path ] || . ~/builder-path
cat <<META > /opt/bowline/etc/builder-info
BUILDER_IMAGE=${BUILDER_IMAGE}
GHC_VERSION=$(ghc --numeric-version)
CABAL_VERSION=$(cabal --numeric-version)
CLONE_URL=${CLONE_URL}
DNSEXT_REV=${DNSEXT_REV}
PRIVKEY_ALG=${PRIVKEY_ALG}
PRIVKEY_ALGOPT=${PRIVKEY_ALGOPT}
CHAIN_SUBJ_CN=${CHAIN_SUBJ_CN}
GHC_OPTIMIZE=${GHC_OPTIMIZE}
PULL_REQ_NUMBER=${PULL_REQ_NUMBER}
META
EOF


FROM debian:${DEBIAN_TAG}

ARG BUILDER_IMAGE
ARG GHC_VERSION
#ARG CABAL_VERSION

ARG CLONE_URL
ARG DNSEXT_REV
ARG PULL_REQ_NUMBER

ARG PRIVKEY_ALG
ARG PRIVKEY_ALGOPT
ARG CHAIN_SUBJ_CN

ARG GHC_OPTIMIZE

LABEL build-arg.BUILDER_IMAGE=${BUILDER_IMAGE}
LABEL build-arg.GHC_VERSION=${GHC_VERSION}
#LABEL build-arg.CABAL_VERSION=${CABAL_VERSION}
LABEL build-arg.CLONE_URL=${CLONE_URL}
LABEL build-arg.DNSEXT_REV=${DNSEXT_REV}
LABEL build-arg.PULL_REQ_NUMBER=${PULL_REQ_NUMBER}
LABEL build-arg.PRIVKEY_ALG=${PRIVKEY_ALG}
LABEL build-arg.PRIVKEY_ALGOPT=${PRIVKEY_ALGOPT}
LABEL build-arg.CHAIN_SUBJ_CN=${CHAIN_SUBJ_CN}
LABEL build-arg.GHC_OPTIMIZE=${GHC_OPTIMIZE}

COPY --from=builder /opt/bowline /opt/bowline
COPY --from=builder /usr/bin/tini /usr/bin/tini
COPY scripts /

WORKDIR /opt/bowline/

## Custom Conf
RUN <<EOF
set -e -x
sed \
    -e 's@^group: .*$@group: nogroup@' \
    -e 's@^disable-v6-ns: .*$@disable-v6-ns: yes@' \
    -e 's@^dns-addrs: .*$@dns-addrs: 0.0.0.0,::@' \
    -e 's@^cnf_dns_addrs: .*$@cnf_dns_addrs: 0.0.0.0@' \
    -e 's@^monitor-addrs: .*$@monitor-addrs: 0.0.0.0,::@' \
    -e 's@^webapi-addr: .*$@webapi-addr: 0.0.0.0@' \
    -e 's@^cert-file: .*$@cert-file: etc/fullchain.pem@' \
    -e 's@^key-file: .*$@key-file: etc/privkey.pem@' \
    < etc/bowline.conf.dist > etc/bowline.conf
chmod 444 etc/bowline.conf
EOF

ENV PATH /opt/bowline/bin:"$PATH"

EXPOSE 53/tcp
EXPOSE 53/udp

EXPOSE 853/tcp
EXPOSE 853/udp

EXPOSE 80/tcp
EXPOSE 443/tcp

EXPOSE 10023/tcp

EXPOSE 8080/tcp

ENTRYPOINT ["/usr/bin/tini", "--", "/bowline.sh"]
