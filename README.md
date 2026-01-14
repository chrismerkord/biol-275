# BIOL 275 – Biostatistics

Public course website built with Quarto and published via GitHub Pages.

URL: https://chrismerkord.github.io/biol-275/

## Updating the R Environment (renv)

This repository uses **renv** to ensure a reproducible R package environment for local development and GitHub Actions builds.

### When to update renv

Update the renv lockfile whenever you:

- Install a new R package

- Upgrade or remove an existing package

- Change package versions during development

### Workflow

1. Work in the project as usual (install, update, or remove packages).

2. Update the lockfile:

   ```r
   renv::snapshot()
   ```
   
3. Review the changes to `renv.lock`.

4. Commit the updated `renv.lock` (and any related files).

### Restoring the environment

To recreate the exact package environment on a new machine:

```r
renv::restore()
```

## tlm-styles Submodule

This repository uses the `tlm-styles` Git submodule to manage shared Quarto styles across multiple course repositories.

The `tlm-styles` submodule is located in the `tlm-styles/` folder and contains:

```
_brand.yml            # Quarto brand tokens (fonts, colors)
tlm-theme.scss          # main SCSS theme file
```

See the [tlm-styles README](tlm-styles/README.md) for details on using and updating the submodule.
