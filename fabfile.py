from datetime import datetime
from time import sleep

from fabric.api import task
from fabric.api import run
from fabric.api import env
from fabric.context_managers import cd
from fabric.context_managers import prefix
from fabric.context_managers import settings
from fabric.colors import green
from fabric.colors import red

ROOT = '/srv'
BACKUP_DIR = ROOT + '/backups/machete'
DEPLOYMENT_DIR = ROOT + '/machete'
SOURCE_DIR = ROOT + '/machete_src'

ENV_PREFIX = 'source /home/huseyin/erlang/171/activate'

env.hosts = ['site']


def gp(msg):
    print(green(msg))


def gok():
    gp('ok')


def rp(msg):
    print(red(msg))


def rok():
    rp(('ok'))


def backup_suffix():
    """
    Retuns time based backup suffix: '_2014_07_27_13_59_57_507910'
    """
    return datetime.now().strftime('_%Y_%m_%d_%H_%M_%S_%f')


@task
def start():
    with cd(DEPLOYMENT_DIR):
        with prefix(ENV_PREFIX):
            gp('Starting machete')
            with settings(warn_only=True):
                run('bin/machete start')
            sleep(5)
            gp('Waiting 5 seconds for machete to start')
            gok()


@task
def backup_mnesia():
    suffix = backup_suffix()
    full_file_name = BACKUP_DIR + '/machete_mnesia_backup' + suffix + '.dub'
    file_name_txt = 'machete_mnesia_backup' + suffix + '.txt'
    full_file_name_txt = BACKUP_DIR + '/' + file_name_txt

    with cd(DEPLOYMENT_DIR):
        with prefix(ENV_PREFIX):
            gp('Starting binary mnesia backup')
            run('bin/machete backup ' + full_file_name)
            gok()
            gp('Starting txt mnesia backup')
            run('bin/machete backup_to_txt ' + full_file_name_txt)
    # gzip text backup file
    with cd(BACKUP_DIR):
        with prefix(ENV_PREFIX):
            run('gzip %s' % (file_name_txt,))
    gok()


@task
def make_release():
    with cd(SOURCE_DIR):
        with prefix(ENV_PREFIX):
            gp('pulling latest changes from github')
            run('git pull origin master')
            gok()
            gp('starting to make release')
            run('make release')
            run('mv machete.tar.gz ../')
            gok()


def replace_source():
    with cd(ROOT):
        with prefix(ENV_PREFIX):
            rp('Stoping machete server')
            run('machete/bin/machete stop')
            rp('Waiting 10 seconds for machete to stop')
            sleep(10)
            rok()
            rp('removing old source code')
            run('rm -rf machete_old')
            run('mv machete machete_old')
            rok()
            rp('Extracting release')
            run('tar -xzvf machete.tar.gz')
            rok()
            rp('Starting new server')
            run('machete/bin/machete start')
            rp('Waiting 10 seconds for machete to start')
            sleep(10)
            rok()


@task
def restore_mnesia():
    rp('restore mnesia backup')
    with cd(BACKUP_DIR):
        output = run('ls *.dub')
        files = output.split()
        assert len(files) > 0, "No backup file exists"
        last_file = sorted(files, reverse=True)[0]
        rp('File_name = %s' % last_file)
        full_file_name = BACKUP_DIR + '/' + last_file

    with cd(DEPLOYMENT_DIR):
        with prefix(ENV_PREFIX):
            # run('cp %s machete_backup.dub' % full_file_name)
            run('pwd')
            run('bin/machete restore %s' % full_file_name)
    rok()


@task
def backup_bower_components():
    with cd(DEPLOYMENT_DIR + '/assets'):
        gp('starting to backup bower components')
        run('tar -czvf bower_components.tar.gz bower_components')
        run('mv bower_components.tar.gz %s/' % ROOT)
        gok()


@task
def restore_bower_components():
    with cd(DEPLOYMENT_DIR + '/assets'):
        rp('starting_to_restore_bower_components')
        run('mv %s/bower_components.tar.gz .' % ROOT)
        run('tar -xzvf bower_components.tar.gz')
        rok()


@task
def deploy():
    start()
    make_release()
    backup_mnesia()
    backup_bower_components()
    replace_source()
    start()
    restore_bower_components()
    restore_mnesia()
    gp('Deoployment successfully completed')
