# -*- coding: utf-8 -*-
'''
    HTTP file handlers.
'''

# This file is part of citadel.

# Distributed under the terms of the last AGPL License.
# The full license is in the file LICENCE, distributed as part of this software.


__author__ = 'Team Machine'


import uuid
import logging
import ujson as json
from tornado import gen
from tornado import web
from citadel.schemas import files as models
from citadel.system import files
from citadel.tools import errors, str2bool, check_json
from citadel.handlers import BaseHandler
from collections import OrderedDict


class Handler(files.Files, BaseHandler):
    '''
        HTTP request handlers
    '''

    @gen.coroutine
    def head(self, account=None, file_uuid=None, page_num=0):
        '''
            Head files
        '''
        # request query arguments
        query_args = self.request.arguments
        # get the current frontend username from cookie
        # username = self.get_username_cookie()
        # get the current frontend username from token
        # username = self.get_username_token()
        username = False
        # if the user don't provide an account we use the frontend username as last resort
        account = (query_args.get('account', [username])[0] if not account else account)
        # query string checked from string to boolean
        checked = str2bool(str(query_args.get('checked', [False])[0]))
        # getting pagination ready
        page_num = int(query_args.get('page', [page_num])[0])
        # rage against the finite state machine
        status = 'all'
        # are we done yet?
        done = False
        # init message on error
        message = {'error':True}
        # init status that match with our message
        self.set_status(400)
        # check if we're list processing
        if not file_uuid:
            message = yield self.get_file_list(account, start, end, lapse, status, page_num)
            self.set_status(200)
        # single file received
        else:
            # first try to get stuff from cache
            file_uuid = file_uuid.rstrip('/')
            # get cache data
            message = self.cache.get('files:{0}'.format(file_uuid))
            if message is not None:
                logging.info('files:{0} done retrieving!'.format(file_uuid))
                self.set_status(200)
            else:
                message = yield self.get_file(account, file_uuid)
                if self.cache.add('files:{0}'.format(file_uuid), message, 1):
                    logging.info('new cache entry {0}'.format(str(file_uuid)))
                    self.set_status(200)
        # so long and thanks for all the fish
        self.finish(message)

    @gen.coroutine
    def get(self, account=None, file_uuid=None, start=None, end=None, page_num=1, lapse='hours'):
        '''
            Get files
        '''
        # request query arguments
        query_args = self.request.arguments
        # get the current frontend username from cookie
        # username = self.get_username_cookie()
        # get the current frontend username from token
        # username = self.get_username_token()
        username = False
        # if the user don't provide an account we use the frontend username as last resort
        account = (query_args.get('account', [username])[0] if not account else account)
        # query string checked from string to boolean
        checked = str2bool(str(query_args.get('checked', [False])[0]))
        # getting pagination ready
        page_num = int(query_args.get('page', [page_num])[0])
        # rage against the finite state machine
        status = 'all'
        # are we done yet?
        done = False
        # init message on error
        message = {'error':True}
        # init status that match with our message
        self.set_status(400)
        # check if we're list processing
        if not file_uuid:
            message = yield self.get_file_list(account, start, end, lapse, status, page_num)
            self.set_status(200)
        # single file received
        else:
            # first try to get stuff from cache
            file_uuid = file_uuid.rstrip('/')
            # get cache data
            message = self.cache.get('files:{0}'.format(file_uuid))
            if message is not None:
                logging.info('files:{0} done retrieving!'.format(file_uuid))
                self.set_status(200)
            else:
                message = yield self.get_file(account, file_uuid)
                if self.cache.add('files:{0}'.format(file_uuid), message, 1):
                    logging.info('new cache entry {0}'.format(str(file_uuid)))
                    self.set_status(200)
        # so long and thanks for all the fish
        self.finish(message)

    @gen.coroutine
    def post(self):
        '''
            Create file
        '''
        struct = yield check_json(self.request.body)
        format_pass = (True if struct and not struct.get('errors') else False)
        if not format_pass:
            self.set_status(400)
            self.finish({'JSON':format_pass})
            return
        # request query arguments
        query_args = self.request.arguments
        # get account from new file struct
        account = struct.get('account', None)
        # get the current frontend username from cookie
        # username = self.get_username_cookie()
        # get the current frontend username from token
        # username = self.get_username_token()
        username = False
        # if the user don't provide an account we use the username
        account = (query_args.get('account', [username])[0] if not account else account)
        # execute new file struct
        file_uuid = yield self.new_file(struct)
        logging.warning(file_uuid)
        # complete message with receive uuid.
        message = {'uuid':file_uuid}
        if 'error' in message['uuid']:
            scheme = 'file'
            reason = {'duplicates': [
                (scheme, 'account'),
                (scheme, 'uuid')
            ]}
            message = yield self.let_it_crash(struct, scheme, message['uuid'], reason)
            self.set_status(400)
        else:
            self.set_status(201)
        self.finish(message)

    @gen.coroutine
    def patch(self, file_uuid):
        '''
            Modify file
        '''
        struct = yield check_json(self.request.body)
        format_pass = (True if not dict(struct).get('errors', False) else False)
        if not format_pass:
            self.set_status(400)
            self.finish({'JSON':format_pass})
            return
        account = self.request.arguments.get('account', [None])[0]
        if not account:
            # if not account we try to get the account from struct
            account = struct.get('account', None)
        # remove query string flag
        remove = self.request.arguments.get('remove', False)
        if not remove :
            result = yield self.modify_file(account, file_uuid, struct)
        else:
            result = yield self.modify_remove(account, file_uuid, struct)
        if not result:
            self.set_status(400)
            system_error = errors.Error('missing')
            error = system_error.missing('file', file_uuid)
            self.finish(error)
            return
        self.set_status(200)
        self.finish({'message': 'update completed successfully'})

    @gen.coroutine
    def delete(self, file_uuid):
        '''
            Delete file
        '''
        query_args = self.request.arguments
        account = query_args.get('account', [None])[0]
        result = self.cache.delete('file:{0}'.format(file_uuid))
        result = yield self.remove_file(account, file_uuid)
        if not result:
            self.set_status(400)
            system_error = errors.Error('missing')
            error = system_error.missing('file', file_uuid)
            self.finish(error)
            return
        self.set_status(204)
        self.finish()

    @gen.coroutine
    def options(self, file_uuid=None):
        '''
            Resource options
        '''
        self.set_header('Access-Control-Allow-Origin', '*')
        self.set_header('Access-Control-Allow-Methods', 'HEAD, GET, POST, PATCH, DELETE, OPTIONS')
        self.set_header('Access-Control-Allow-Headers', ''.join(('Accept-Language,',
                        'DNT,Keep-Alive,User-Agent,X-Requested-With,',
                        'If-Modified-Since,Cache-Control,Content-Type,',
                        'Content-Range,Range,Date,Etag')))
        # allowed http methods
        message = {
            'Allow': ['HEAD', 'GET', 'POST', 'PATCH', 'DELETE', 'OPTIONS']
        }
        # resource parameters
        parameters = {}
        # mock your stuff
        stuff = models.File.get_mock_object().to_primitive()
        for k, v in stuff.items():
            if v is None:
                parameters[k] = str(type('none'))
            else:
                parameters[k] = str(type(v))
        # after automatic madness return description and parameters
        parameters['labels'] = 'array/string'
        # end of manual cleaning
        POST = {
            "description": "Create file",
            "parameters": OrderedDict(sorted(parameters.items(), key=lambda t: t[0]))
        }
        # filter single resource
        if not file_uuid:
            message['POST'] = POST
        else:
            message['Allow'].remove('POST')
            message['Allow'].append('PATCH')
            message['Allow'].append('DELETE')
        self.set_status(200)
        self.finish(message)
