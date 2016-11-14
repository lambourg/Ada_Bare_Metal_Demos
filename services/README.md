Overview
--------

This folder contains a prototype version of Filesystem support library
written in Ada. This is not strictly speaking part of this demo repository
and is meant to be moved to a standalone repository at some point.

The reason of its presence here is to allow filesystem access within some
of the demos (sdcard and wav_reader demos).

Features
--------

This library is composed of several components:
* filesystem: an interface definition for filesystem support
* filesystem.MBR: support for Master Boot Record partitions
* filesystem.VFS: support for virtual mount points
* filesystem.FAT: support for FAT16/FAT32

Status
------

MBR and VFS should work OK, they're very simple packages.

FAT support for FAT32 should not destroy completely your data, however:
* it is strongly advised to use the library on a support where no important
  data is present
* in particular, the write mode should be ok most of the time, but is still
  very experimental
* FAT16 support is still work in progress.

Roadmap
-------

* provide a common handle memory pool, to allow the usage of a static pool
  of generic handles
* add support for GUID partitions
* strengthen and add more testing of the write functionality
* add support for creating a MBR from scratch, and formatting a new partition.

License
-------

The license of this library is GPL+Runtime exception.
