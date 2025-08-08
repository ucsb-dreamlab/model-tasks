# Base image
FROM rocker/r2u

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install nvm, Node.js, and Gemini CLI as root
ENV NVM_DIR="/root/.nvm"
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash && \
    . "$NVM_DIR/nvm.sh" && \
    nvm install 22 && \
    npm install -g @google/gemini-cli

# Add nvm to PATH for root
ENV PATH $NVM_DIR/versions/node/v22.17.0/bin:$PATH

# Install Claude CLI
RUN curl -fsSL https://claude.ai/install.sh | bash
