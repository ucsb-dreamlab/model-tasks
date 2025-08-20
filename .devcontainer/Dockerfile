# Base image
FROM docker.io/rocker/r2u

# Install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
  python3 \
  make \
  g++ \
  man-db \
  curl \
  dnsutils \
  less \
  jq \
  bc \
  gh \
  git \
  unzip \
  rsync \
  ripgrep \
  procps \
  psmisc \
  lsof \
  socat \
  ca-certificates \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Install nvm, Node.js, and Gemini CLI as root
ENV NVM_DIR="/root/.nvm"
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash && \
    . "$NVM_DIR/nvm.sh" && \
    nvm install 22 && \
    npm install -g @google/gemini-cli

# Install Claude CLI
RUN curl -fsSL https://claude.ai/install.sh | bash


RUN echo "#!/bin/env bash\n. \"/root/.nvm/nvm.sh\"\ngemini" > /entrypoint.sh \
	&& chmod a+x /entrypoint.sh

WORKDIR /workspace

CMD ["/entrypoint.sh"]
