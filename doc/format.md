# EXS File Format

Between reading (this source)[# https://bitbucket.org/vonred/exstosfz/src/default/exs2sfz.py?at=default&fileviewer=file-view-default] and
reverse engineering a bit, here's what I have figured out.

## Basic structure

The core exs file is a binary file, little endian, set up in chunks.

Each chunk starts with a common 84 bit header then payload. That header has format (in "bits / meaning")

* 0-3 type signature
* 4-7 size
* 8-11 chunk id
* 12-19 (unknown - but seems correlated with type)
* 20-84 UTF-8 encoded 0 terminated string for name

After this, another "size" bytes appear whose interpretation depends on the type signature, and then another chunk until the file ends.
