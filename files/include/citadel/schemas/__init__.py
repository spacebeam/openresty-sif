# -*- coding: utf-8 -*-
'''
    base message models.
'''

# This file is part of luna.

# Distributed under the terms of the last AGPL License.
# The full license is in the file LICENCE, distributed as part of this software.


__author__ = 'Team Machine'


from schematics import models
from schematics import types
from schematics.types import compound


class BaseResult(models.Model):
    '''
        Base result
    '''
    count = types.IntType()
    page = types.IntType()
    results = compound.ListType(types.StringType())
