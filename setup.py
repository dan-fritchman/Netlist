"""
# Setup Script

Derived from the setuptools sample project at
https://github.com/pypa/sampleproject/blob/main/setup.py

"""

# Always prefer setuptools over distutils
from setuptools import setup, find_packages
import pathlib

here = pathlib.Path(__file__).parent.resolve()

# Get the long description from the README file
long_description = (here / "readme.md").read_text(encoding="utf-8")

setup(
    name="netlist",
    version="0.1.0",
    description="Circuit Netlist Generation and Parsing",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/dan-fritchman/Netlist",
    author="Dan Fritchman",
    author_email="dan@fritch.mn",
    packages=find_packages(),
    python_requires=">=3.8, <4",
    install_requires=["pydantic==1.8.2",],
    extras_require={
        "dev": [
            "pytest==5.2",
            "coverage",
            "pytest-cov",
            "black==19.10b0",
            "click==8.0.1",  # This is transitive on `black`, but required for the CLI to work
            "twine",
        ]
    },
)
