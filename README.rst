machete
=======
A url shortener.

Build Dependencies
==================

* Bower
* Erlang 17.0 or higher
  (I did not test other version. it should be working on lesser versions too)

How to build
============

First of all, get the source code:

::

   $ git clone git://github.com/huseyinyilmaz/machete.git

Then build frontend dependencies with npm

::

   $ npm run build

If you want to start server without building the release.

::

    $ make deps
    $ make start

After starting the server goto http://localhost:8767/assets/index.html


You can build the release with this command

::

   $ make release

This command will create machete.tar.gz file on project root.

Configuration
=============

Before initializing the database, you might want to change default configuration. There is two configuration file that can be changed.

[project_root]/releases/[release_number]/sys.conf
-------------------------------------------------
This configuration file has application specific files. From this file you can change:

* IP and port settings
* DB directory
* LOG configuration

[project_root]/releases/[release_number]/vm.args
-------------------------------------------------
This configuration file has virtual machine specific settings. From this file you can change:

* Name of the machete node. (This is important if you want to run machete as cluster)
* Set connection cookie. (All nodes in cluster has to have same cookie)
* Set maximum number of concurrent ports/sockets.

Setting up database
===================

If you want to run machete as a single node. Run

::
   $ bin/machete create_schema

This will create database schema on project root. If you need to change the place that schema is created use sys.conf configuration file as described above.

Setting up a cluster
====================

First of all make sure that all nodes in the cluster has their own node name (vm.args file). Than start one of the nodes with

::
   $ bin/machete start

After that create schema on that node:

::
   $ bin/machete create_schema

Than start other nodes and run

::
   $ bin/machete start
   $ bin/machete connect [machete_master_node_name@127.0.0.1]

This will connect your current node to cluster. You can use any node in the cluster to connect. You do not really need to use the node that schema is created.

Deploying release
=================

Machete release is a self contained http server. Just untar machete.tar.gz to somewhere and call following command the start server.

::

    $ bin/machete start

This will run the server on default port 8767. Log files will be under [project_root]/log directory. Database will be stored under [project_root]/mnesia directory. Application config file is [project_root]/releases/[release_number]/sys.conf

Here are some other console commands that are provided.

::

   $ bin/machete stop # to stop server
   $ bin/machete backup file_name.dub # to backup to file_name.dub file
   $ bin/machete restore file_name.dub # to restore from file_name.dub file
   $ bin/machete txt_backup file_name.txt # backup in human readable format (educational purposes)
   $ bin/machete restore_from_txt_backup file_name.txt # restore from human readable format

Note: Txt backup is not suggested to use in production. I have just added it for an experiment.
