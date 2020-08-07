# -*- coding: utf-8 -*-
'''
    periodic system functions.
'''

# This file is part of luna.

# Distributed under the terms of the last AGPL License.
# The full license is in the file LICENCE, distributed as part of this software.


__author__ = 'Team Machine'


import os
import glob
import binascii
from hashlib import md5
import logging
import ujson as json
from tornado import gen
from tornado import httpclient

from subprocess import call, Popen, PIPE

from sloth.tools import clean_response, get_search_list


httpclient.AsyncHTTPClient.configure('tornado.curl_httpclient.CurlAsyncHTTPClient')
http_client = httpclient.AsyncHTTPClient()


@gen.coroutine
def process_uuid_parts(payload_uuid, file_uuid):
    '''
        process split parts
    '''
    # hopefully asynchronous handle function request
    def handle_request(response):
        '''
            Request Async Handler
        '''
        if response.error:
            logging.error(response.error)
            got_response.append({'error':True, 'message': response.error})
        else:
            got_response.append(json.loads(response.body))

    for filename in glob.iglob('/tmp/{0}.part*'.format(payload_uuid), recursive=True):
        got_response = []
        message = {
            'count': 0,
            'results': []
        }
        url = 'https://nonsense.ws/chunks/'  # Don't hardcode shit like this!
        with open(filename, 'rb') as f:
            content = f.read()
        try:
            http_client.fetch(
                url,
                method="POST",
                body=json.dumps({
                    'payload': binascii.hexlify(content),
                    'payload_uuid': payload_uuid,
                    'uuid': file_uuid
                }),
                headers={'Content-Type':'application/json'},
                callback=handle_request
            )
            while len(got_response) == 0:
                # Yo, don't be careless with the time!
                yield gen.sleep(0.0020)
            # get stuff from response
            response = got_response[0]
            logging.warning("its time to finish this payload {0} from file id {1}".format(payload_uuid, file_uuid))
            # stuff and stuff (=
            logging.warning('next uuid returning from Erlang')
            logging.warning(response['uuid'])
            # just for now log the shit out of what we're processing
        except Exception as error:
            logging.warning(error)
        logging.warning(message.get('count'))

@gen.coroutine
def process_payload_uuid(uuid):
    '''
        process new upload uuid
    '''
    process = Popen(['file', '/tmp/{0}'.format(uuid), "."], stdout=PIPE)
    (output, err) = process.communicate()
    # Yo, wait for some shit.
    exit_code = process.wait()
    try:
        logging.warning(output)
        command = 'split -a 5 -b 256K -d /tmp/{0} /tmp/{1}.part'.format(uuid, uuid)
        status = call(command, shell=True)
    except FileNotFoundError:
        logging.warning('there is not file {0}'.format(uuid))

@gen.coroutine
def check_new_uploads(account="pebkac", status="upload", page_num=1, page_size=100):
    '''
        Check new uploads
    '''
    domain = 'nonsense.ws'
    search_index = 'sloth_file_index'
    query = 'uuid_register:*'
    filter_query = 'account_register:{0}&fq=status_register:{1}'.format(account,status)
    start_num = page_size * (page_num - 1)
    url = get_search_list(domain, search_index, query, filter_query, start_num, page_size)
    got_response = []
    message = {
        'count': 0,
        'page': page_num,
        'results': []
    }
    # ignore riak fields
    IGNORE_ME = ["_yz_id","_yz_rk","_yz_rt","_yz_rb"]
    # hopefully asynchronous handle function request
    def handle_request(response):
        '''
            Request Async Handler
        '''
        if response.error:
            logging.error(response.error)
            got_response.append({'error':True, 'message': response.error})
        else:
            got_response.append(json.loads(response.body))
    try:
        http_client.fetch(
            url,
            callback=handle_request
        )
        while len(got_response) == 0:
            # Yo, don't be careless with the time!
            yield gen.sleep(0.0020)
        # get stuff from response
        stuff = got_response[0]
        if stuff['response']['numFound']:
            message['count'] += stuff['response']['numFound']
            for doc in stuff['response']['docs']:
                message['results'].append(clean_response(doc, IGNORE_ME))
        else:
            logging.error('there is probably something wrong!')
    except Exception as error:
        logging.warning(error)
    logging.warning(message.get('count'))
    for yo in message.get('results'):
        ooo = yield process_payload_uuid(yo.get('payload'))
        aaa = yield process_uuid_parts(yo.get('payload'), yo.get('uuid'))

@gen.coroutine
def check_erlang_node(
        home='/opt/sloth',
        erlang_release='/opt/sloth/_rel/sloth_release/bin/sloth_release',
        circusd="/etc/init.d/circusd",
        max_count=5):
    '''
        Checks for an active Erlang/OTP node
    '''
    os.environ['HOME'] = home
    von_count = 0
    running = False
    process = Popen([erlang_release, "ping", "."], stdout=PIPE)
    (output, err) = process.communicate()
    exit_code = process.wait()
    if b'not responding to pings' in output:
        logging.error(output)
        process = Popen([erlang_release, "start", "."], stdout=PIPE)
        (output, err) = process.communicate()
        exit_code = process.wait()
        logging.error(output)
    elif b'pong' in output:
        if not running:
            logging.warning('pong!')
            running = True
    else:
        von_count += 1
        if von_count > max_count:
            circus = Popen([circusd, "stop", "."], stdout=PIPE)
            (output, err) = circus.communicate()
            logging.error('Crash after trying {0} times!'.format(max_count))
