ff = Proto("flowfwd", "CSU33031 FF Protocol")

local type_names = {
    "Acknowledgment",
    "Peer Address",
    "Source Address",
    "Destination Address",
    "Client Port",
    "Payload"
}

local ack_names = {
    "OK",
    "Routing Packet...",
    "Forwarding Packet...",
    "Packet Too Long!",
    "Invalid Packet!",
    "Invalid Datatype!",
    "Duplicate Datatype!",
    "Unrecognised Packet Archetype!",
    "No Client Receivers!",
    "Destination Not Found!",
    "Unable To Send Forward!"
}

function ff.dissector(buffer, pinfo,tree)
    local length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = "FF"
    local subtree = tree:add(ff, buffer(), "FF Protocol")

    local i = 0
    while i < length do
        local t = buffer(i + 0, 1)
        local l = buffer(i + 1, 1)
        local v = buffer(i + 2, l:uint())
        local len = 2 + l:uint()

        local name = type_names[t:uint() + 1]
        local value = v:string()
        if t:uint() == 0 then
            value = v:uint() .. " (" .. ack_names[v:uint() + 1] .. ")"
        end

        local tlv_subtree = subtree:add(buffer(i, len), name)
        tlv_subtree:add(t, "Type: " .. t:uint() .. " (" .. name .. ")")
        tlv_subtree:add(l, "Length: " .. l:uint())
        tlv_subtree:add(v, "Value: " .. value)

        i = i + len
    end
end

udp_table = DissectorTable.get("udp.port")
udp_table:add(51510, ff)
