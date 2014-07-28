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
   $ bin/machete backup_to_txt file_name.txt # backup in human readable format (educational purposes)
   $ bin/machete restore_from_txt file_name.txt # restore from human readable format
