# Building the documentation

## Prerequisites

Please make sure you have Python 3 installed. If not - follow instructions from the [official Python page](https://www.python.org/downloads/).

To install the documentation generator `sphinx-doc` on your platform, please follow [these instructions](https://www.sphinx-doc.org/en/master/usage/installation.html).

## Preparation

Execute these commands:

```
sphinx-build -b html . builddir
pip3 install sphinx-rtd-theme
pip3 install recommonmark
pip3 install sphinx_markdown_tables --user
pip3 install sphinxemoji --user
```

## Building documentation

Use this command to build documentation:

```
sphinx-build doc html
```

Now open the documentation at `html/index.html` page.
