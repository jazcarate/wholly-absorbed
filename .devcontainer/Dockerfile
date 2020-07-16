FROM nixos/nix

# Location and expected SHA for common setup script - SHA generated on release
COPY scripts ./scripts

# Configure apt and install packages
RUN apk update \
    && /bin/ash ./scripts/common-alpine.sh

# Add cache for nix
RUN nix-env --install --attr cachix -f https://cachix.org/api/v1/install \
    && cachix use iohk \
    && cachix use ghcide-nix