# Use the pre-built base image with dependencies
FROM ghcr.io/harfangk/blog-hakyll-netlify-base:latest

# Copy source code
COPY . .

# Build the project (dependencies are already built in base image)
RUN stack build

# Build the website
RUN stack exec blog-hakyll-netlify-exe build

# The _site directory contains the generated website