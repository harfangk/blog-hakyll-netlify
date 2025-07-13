# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

This is a Haskell blog built with Hakyll and Stack. Use these commands for development:

```bash
# Build the site generator
stack build

# Generate the site
stack exec blog-hakyll-netlify-exe build

# Watch for changes and rebuild automatically
stack exec blog-hakyll-netlify-exe watch

# Clean build artifacts
stack exec blog-hakyll-netlify-exe clean
```

Note: On macOS, install `pkgconf` before building: `brew install pkgconf`

## Architecture Overview

This is a multilingual static blog built with Hakyll that supports English, Korean, and German content.

### Key Components

- **site.hs**: Main site generator with Hakyll rules for processing content
- **I18n.hs**: Internationalization module handling language-specific routing and text
- **posts/**: Content organized in dated directories, each containing language-specific markdown files (e.g., `en.md`, `ko.md`, `de.md`)
- **templates/**: HTML templates for rendering pages
- **css/main.scss**: SASS stylesheet compiled to CSS during build

### Content Structure

Posts are organized as: `posts/YYYY-MM-DD-title/LANG.md` where LANG is the language code. The site generator:

1. Scans post directories to identify available languages per post
2. Generates language-specific URLs and navigation
3. Creates paginated index pages for each language
4. Builds RSS/Atom feeds per language
5. Handles multilingual cross-references

### Key Features

- Multilingual support with automatic language detection and routing
- Pagination for post listings (10 posts per page)
- RSS and Atom feeds for each language
- SASS compilation with compression
- Netlify deployment with redirect rules (_redirects file)

### Important Notes

- The main languages are English and Korean (`I18n.mainLangs`)
- German is supported but not included in main feeds
- Site compiles SCSS using external `sass` command (requires sass to be installed)
- Content snapshots are saved for feed generation
- Language-specific contexts provide translated UI text