# Base image
FROM rocker/r2u

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js and npm for Gemini CLI
RUN apt-get update && apt-get install -y ca-certificates curl gnupg
RUN mkdir -p /etc/apt/keyrings
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
RUN echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list
RUN apt-get update && apt-get install nodejs -y

# Install Gemini CLI
RUN npm install -g @google/gemini-cli

# Install Claude CLI
RUN curl -fsSL https://claude.ai/install.sh | bash

# Set a non-root user
USER rstudio
WORKDIR /home/rstudio

# Expose port for RStudio
EXPOSE 8787
