# Stage 0: App image
# Use the Erlang image as the builder stage
FROM erlang:21-alpine AS builder

# Install necessary build tools
RUN apk add --no-cache git build-base

# Create a directory for building
RUN mkdir /buildroot
# Set the working directory
WORKDIR /buildroot

# Copy project files into the container
COPY . /buildroot/web_rtp
# Change working directory to the project directory
WORKDIR /buildroot/web_rtp

# Build the release using Rebar3
RUN rebar3 as prod release

# Stage 1: Final image
FROM alpine:3.13

# Install some libs
RUN apk add --no-cache git \
    make gcc musl-dev\
    openssl ortp-dev\
    ncurses-libs bctoolbox-dev\
    libstdc++ \
    ffmpeg

# Copy Makefile
WORKDIR /buildroot
RUN mkdir /c_src
COPY ./c_src/Makefile /buildroot/web_rtp/c_src/Makefile

# Copy rtpsent.c
COPY ./c_src/rtpsend.c /buildroot/web_rtp/c_src/rtpsend.c

# Start Make for comp
WORKDIR /buildroot/web_rtp/c_src
RUN make

# Change working directory to the project directory
WORKDIR /buildroot/web_rtp

# Copy the built release from the builder stage
COPY --from=builder /buildroot/web_rtp/_build/prod/rel/web_rtp /web_rtp

# Expose ports
EXPOSE 8080
EXPOSE 8443

# Run the application
CMD ["/web_rtp/bin/web_rtp", "foreground"]