# -*- coding: utf-8 -*-
'''
    File storage service around OpenResty and Riak KV.
'''
# This file is part of citadel.

# Distributed under the terms of the last AGPL License.
# The full license is in the file LICENCE, distributed as part of this software.


__author__ = 'Team Machine'


import zmq
import uuid
import riak
import logging
import pylibmc as mc
from tornado import ioloop
from tornado.ioloop import PeriodicCallback as Cast
from tornado import gen, web
from tornado.web import RequestHandler
from citadel.handlers import files
from citadel.tools import options, periodic


def main():
    '''
        main function
    '''
    # daemon options
    opts = options.options()
    # Set memcached backend
    memcache = mc.Client(
        [opts.memcached_host],
        binary=opts.memcached_binary,
        behaviors={
            "tcp_nodelay": opts.memcached_tcp_nodelay,
            "ketama": opts.memcached_ketama
        }
    )
    # riak key-value
    kvalue = riak.RiakClient(host=opts.riak_host, pb_port=8087)
    # memcached
    cache = memcache
    # current db
    db = kvalue
    # system uuid
    system_uuid = uuid.uuid4()
    # logging system spawned
    logging.info('Spawned sloth system {0} '.format(system_uuid))
    # logging riak settings
    logging.info('Riak server: {0}:{1}'.format(opts.riak_host, opts.riak_port))
    # check for cache
    cache_enabled = opts.cache_enabled
    if cache_enabled:
        logging.info('Memcached server: {0}:{1}'.format(opts.memcached_host, opts.memcached_port))
    # web application server daemon
    application = web.Application(
        [
            (r'/files/page/(?P<page_num>\d+)/?', files.Handler),
            (r'/files/(?P<file_uuid>.+)/?', files.Handler),
            (r'/files/?', files.Handler),
        ],
        db = db,
        cache = cache,
        kvalue = kvalue,
        debug = opts.debug,
        domain = opts.domain,
        page_size = opts.page_size,
    )
    # set after periodic definition
    check_erlang_node = Cast(periodic.check_erlang_node, 5000)
    check_erlang_node.start()

    # (=

    check_new_uploads = Cast(periodic.check_new_uploads, 10000)
    check_new_uploads.start()
    # Setting up the application server process
    application.listen(opts.port)
    logging.info('Listening on http://{0}:{1}'.format(opts.host, opts.port))
    ioloop.IOLoop.instance().start()

if __name__ == '__main__':
    '''
        Processing files on machines of all ages!
    '''
    main()
