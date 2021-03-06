zoom-cache: A streamable, seekable, zoomable cache file format
==============================================================

[![Build Status](https://secure.travis-ci.org/kfish/zoom-cache.png)](http://travis-ci.org/kfish/zoom-cache)

These are some of the goals of the zoom-cache format:

Writing
-------

    * can be written to in streaming manner, with no backseeking

Reading
-------

    * can be read in one pass with no backseeking
    * can be seeked on time-wise
    * sections can be extracted
    * can be read at varying zoom levels

Data
----

    * Support multiple data types (float/double, int, bool, char)
    * support multiple aggregation types for zooming (min/max, mean, rms, all/any,
        text summary)
    * Constant or variable bitrates streams

Library
-------

    * Simple stream writing interface
    * Simple stream reading interface
    * Read seek interface
    * Read set-zoom-level interface

Tools
-----

    * Verification
    * Info


Interleaved streams
-------------------

  * Sync points

Index files
-----------

  * A documented way of generating time index files

Exploded form
-------------

  * one file per stream

