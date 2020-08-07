#!/usr/bin/env luajit
local upload = require("resty.upload")
local uuid = require("uuid")
local socket = require("socket")
local cjson = require("cjson")
local http = require("lapis.nginx.http")
-- init uuid random seed
uuid.randomseed(socket.gettime()*10000)
-- local variables
local chunk_size = 4096
local file
local file_name
local file_uuid = uuid()
local von_count = 1
local message = {}
-- files url
local files = "https://api.torchup.net/files/"
local form, err = upload:new(chunk_size)
-- not form exit
if not form then
    ngx.exit(500)
end
-- fruit loop
while true do
    local typ, res, err = form:read()
    if not typ then
        ngx.say("failed to read: ", err)
        return
    end
    if typ == "header" then
        file_name = uuid()
        if file_name then
            file = io.open("/tmp/" .. file_name, "w+")
            if von_count == 1 then
                message["head"] = file_name
            elseif von_count == 2 then
                message["payload"] = file_name
            else
                ngx.say("let it crash")
            end
            von_count = von_count + 1
            if not file then
                ngx.exit(500)
                return
            end
        end
    elseif typ == "body" then
        if file then
            file:write(res)
        end
    elseif typ == "part_end" then
        file:close()
        file = nil
    elseif typ == "eof" then
        message["uuid"] = file_uuid
        message["account"] = "pebkac"
        message["status"] = "upload"
        message["active"] = "False"
        local body, status_code, headers = http.simple({
            url = files,
            method = "POST",
            headers = {
              ["content-type"] = "application/json"
            },
            body = cjson.encode(message)
        })
        ngx.say(body)
        break
    else
        -- do nothing
    end
end
