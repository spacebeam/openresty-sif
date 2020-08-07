# -*- coding: utf-8 -*-
'''
    configuration server options.
'''

# This file is part of luna.

# Distributed under the terms of the last AGPL License.
# The full license is in the file LICENCE, distributed as part of this software.


__author__ = 'Team Machine'


import os
import base64
import uuid
import tornado.options
from tornado.options import parse_config_file


config_path = 'citadel.conf'


def options():
    '''
        configuration server options
    '''
    # Set server configuration
    tornado.options.define('config',
        type=str, help='path to config file',
        callback=lambda path: parse_config_file(path, final=False))
    # debugging
    tornado.options.define('debug',
        default=False, type=bool,
        help=('Turn on autoreload and log to stderr only'))
    # logging dir
    tornado.options.define('logdir',
        type=str, default='log',
        help=('Location of logging (if debug mode is off)'))
    # domain
    tornado.options.define('domain',
        default='*', type=str,
        help='Application domain, e.g: "your_domain.com"')
    # Erlang/OTP release
    tornado.options.define('erlang_release',
        default='/opt/treehouse/_rel/sloth_release/bin/sloth_release',
        type=str, help=('Erlang/OTP release'))
    # solr host
    tornado.options.define('solr',
        default='api.cloudforest.ws', type=str,
        help='Application solr, e.g: "your_custom_solr_search"')
    # Server settings
    tornado.options.define('host',
        default='127.0.0.1', type=str,
        help=('Server hostname'))
    # server port
    tornado.options.define('port',
        default=8888, type=int,
        help=('Server port'))
    # Riak kvalue datastorage settings
    tornado.options.define('riak_host',
        default='127.0.0.1', type=str,
        help=('Riak cluster node'))
    # Riak port
    tornado.options.define('riak_port',
        default=8087, type=int,
        help=('Riak cluster port'))
    # PostgreSQL database settings
    tornado.options.define('sql_host',
        type=str, help=('PostgreSQL hostname or ip address'))
    # SQL port
    tornado.options.define('sql_port',
        default=5432, type=int,
        help=('PostgreSQL port'))
    # SQL database
    tornado.options.define('sql_database',
        type=str, help=('PostgreSQL database'))
    # SQL user
    tornado.options.define('sql_user',
        type=str, help=('PostgreSQL username'))
    # SQL password
    tornado.options.define('sql_password',
        type=str, help=('PostgreSQL username password'))
    # memcache host
    tornado.options.define('memcached_host',
        default='127.0.0.1', type=str,
        help=('Memcached host'))
    # memcache port
    tornado.options.define('memcached_port',
        default=11211, type=int,
        help=('Memcached port'))
    tornado.options.define('memcached_binary',
        default=True, type=bool,
        help=('Memcached binary'))
    tornado.options.define('memcached_tcp_nodelay',
        default=True, type=bool,
        help=('Memcached tcp_nodelay'))
    tornado.options.define('memcached_ketama',
        default=True, type=bool,
        help=('Memcached ketama'))
    tornado.options.define('cache_enabled',
        default=False, type=bool,
        help=('Enable cache'))
    tornado.options.define('page_size',
        default=50, type=int,
        help=('Set a custom page size up to 50'))
    # Parse config file, then command line...
    # so command line switches take precedence
    if os.path.exists(config_path):
        print('Loading %s' % (config_path))
        tornado.options.parse_config_file(config_path)
    else:
        print('No config file at %s' % (config_path))
    tornado.options.parse_command_line()
    result = tornado.options.options
    for required in ('domain', 'host', 'port'):
        if not result[required]:
            raise Exception('%s required' % required)
    return result
