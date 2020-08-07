# -*- coding: utf-8 -*-
'''
    citadel files models and messages.
'''

# This file is part of luna.

# Distributed under the terms of the last AGPL License.
# The full license is in the file LICENCE, distributed as part of this software.

__author__ = 'Team Machine'


import arrow
import uuid
from schematics import models
from schematics import types
from schematics.types import compound
# imports needed for CRDT's prototypes
import riak
import logging
import ujson as json
from riak.datatypes import Map


class File(models.Model):
    '''
        File Data Structure
    '''
    uuid = types.UUIDType(default=uuid.uuid4)
    url = types.StringType()
    account = types.StringType(required=True)
    checksum = types.StringType()
    head = types.StringType()
    payload = types.StringType()
    public = types.BooleanType(default=False)
    labels = compound.ListType(types.StringType())
    hashes = compound.ListType(types.StringType())
    chunks = compound.ListType(types.StringType())
    status = types.StringType()
    created_by = types.StringType()
    created_at = types.TimestampType(default=arrow.utcnow().timestamp)
    updated = types.BooleanType(default=False)
    last_update_by = types.StringType()
    last_update_at = types.TimestampType()
    checked = types.BooleanType(default=False)
    checked_by = types.StringType()
    checked_at = types.TimestampType()
    active = types.BooleanType(default=True)
    watchers = compound.ListType(types.StringType())


class ModifyFile(models.Model):
    '''
        Modify File

        This model is similar to File.

        It lacks of require and default values on it's fields.

        The reason of it existence is that we need to validate
        every input data that came from outside the system, with
        this we prevent users from using PATCH to create fields
        outside the scope of the resource.
    '''
    uuid = types.UUIDType()
    url = types.StringType()
    account = types.StringType()
    checksum = types.StringType()
    head = types.StringType()
    payload = types.StringType()
    public = types.BooleanType()
    labels = compound.ListType(types.StringType())
    hashes = compound.ListType(types.StringType())
    chunks = compound.ListType(types.StringType())
    status = types.StringType()
    created_by = types.StringType()
    created_at = types.TimestampType()
    updated = types.BooleanType()
    last_update_by = types.StringType()
    last_update_at = types.TimestampType()
    checked = types.BooleanType()
    checked_by = types.StringType()
    checked_at = types.TimestampType()
    active = types.BooleanType()
    watchers = compound.ListType(types.StringType())


class FileMap(object):

    def __init__(
        self,
        client,
        bucket_name,
        bucket_type,
        search_index,
        struct
    ):
        '''
            File map structure
        '''
        bucket = client.bucket_type(bucket_type).bucket('{0}'.format(bucket_name))
        bucket.set_properties({'search_index': search_index})
        self.map = Map(bucket, None)
        # start of map structure
        self.map.registers['uuid'].assign(struct.get('uuid', ''))
        self.map.registers['url'].assign(struct.get('url', ''))
        self.map.registers['account'].assign(struct.get('account', ''))
        self.map.registers['checksum'].assign(struct.get('checksum', ''))
        self.map.registers['head'].assign(struct.get('head', ''))
        self.map.registers['payload'].assign(struct.get('payload', ''))
        self.map.registers['public'].assign(struct.get('public', ''))
        self.map.registers['labels'].assign(struct.get('labels',''))
        self.map.registers['hashes'].assign(struct.get('hashes',''))
        self.map.registers['chunks'].assign(struct.get('chunks',''))
        self.map.registers['status'].assign(struct.get('status', ''))
        self.map.registers['created_by'].assign(struct.get('created_by', ''))
        self.map.registers['created_at'].assign(struct.get('created_at', ''))
        self.map.registers['updated'].assign(struct.get('updated', ''))
        self.map.registers['last_update_by'].assign(struct.get('last_update_by', ''))
        self.map.registers['last_update_at'].assign(struct.get('last_update_at', ''))
        self.map.registers['checked'].assign(struct.get('checked', ''))
        self.map.registers['checked_by'].assign(struct.get('checked_by', ''))
        self.map.registers['checked_at'].assign(struct.get('checked_at', ''))
        self.map.registers['active'].assign(struct.get('active', ''))
        self.map.registers['watchers'].assign(struct.get('watchers', ''))
        # end of the map stuff
        self.map.store()

    @property
    def uuid(self):
        return self.map.reload().registers['uuid'].value

    @property
    def account(self):
        return self.map.reload().registers['account'].value

    def to_json(self):
        event = self.map.reload()
        struct = {
            "uuid": event.registers['uuid'].value,
            "url": event.registers['url'].value,
            "account": event.registers['account'].value,
            "checksum": event.registers['checksum'].value,
            "head": event.registers['head'].value,
            "payload": event.registers['payload'].value,
            "public": event.registers['public'].value,
            "labels": event.registers['labels'].value,
            "hashes": event.registers['hashes'].value,
            "chunks": event.registers['chunks'].value,
            "status": event.registers['status'].value,
            "created_by": event.registers['created_by'].value,
            "created_at": event.registers['created_at'].value,
            "updated": event.registers['updated'].value,
            "last_update_by": event.registers['last_update_by'].value,
            "last_update_at": event.registers['last_update_at'].value,
            "checked": event.registers['checked'].value,
            "checked_by": event.registers['checked_by'].value,
            "checked_at": event.registers['checked_at'].value,
            "watchers": event.registers['watchers'].value,
            "active": event.registers['active'].value,
        }
        return json.dumps(struct)

    def to_dict(self):
        event = self.map.reload()
        struct = {
            "uuid": event.registers['uuid'].value,
            "url": event.registers['url'].value,
            "account": event.registers['account'].value,
            "checksum": event.registers['checksum'].value,
            "head": event.registers['head'].value,
            "payload": event.registers['payload'].value,
            "public": event.registers['public'].value,
            "labels": event.registers['labels'].value,
            "hashes": event.registers['hashes'].value,
            "chunks": event.registers['chunks'].value,
            "status": event.registers['status'].value,
            "created_by": event.registers['created_by'].value,
            "created_at": event.registers['created_at'].value,
            "updated": event.registers['updated'].value,
            "last_update_by": event.registers['last_update_by'].value,
            "last_update_at": event.registers['last_update_at'].value,
            "checked": event.registers['checked'].value,
            "checked_by": event.registers['checked_by'].value,
            "checked_at": event.registers['checked_at'].value,
            "watchers": event.registers['watchers'].value,
            "active": event.registers['active'].value,
        }
        return struct
