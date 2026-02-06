# hash:sha256:058a9dde25779ec73b5a097c576d51fa54f00d488a6e38c7d62a08a82fe8ba35
FROM registry.codeocean.com/codeocean/r-studio:r-4.5.1-1.2204.0-rstudio-server-2025.05.1-513-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

COPY postInstall /
RUN /postInstall
