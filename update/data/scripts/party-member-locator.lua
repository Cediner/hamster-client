local api = require("data.scripts.lualib.api")
local members = api.party.members()
local me = api.gob.mygob()
local dist = 25*11

function find_line(me, it)
    local m = (it.y - me.y) / (it.x - me.x)
    return {
        angle = me:angle(it) - (math.pi / 2),
        slope = m,
        offset = it.y - (it.x * m)
    }
end

function intersection(y1, y2)
    local x = (y2.offset - y1.offset) / (y1.slope - y2.slope)
    return api.coord.coord2d(x, y1.slope * x + y1.offset)
end

if #members > 1 then
    for i=1, #members do
        local member = members[i]
        if member.gobid ~= me.id then
            local startpos = me.rc

            -- Get our starting vector
            local curpos = me.rc
            api.widget.wdgmsg(member.view, "click", 1)
            api.mv.wait_for_movement_start()
            local startline = find_line(curpos, me:getDest())
            api.mv.move_to(curpos)
            api.mv.wait_for_movement()

            -- Move perpendicular to the target
            api.mv.move_to(curpos:add(dist*math.cos(startline.angle),
                dist*math.sin(startline.angle)))
            api.mv.wait_for_movement()

            -- Get another vector reference
            curpos = me.rc
            api.widget.wdgmsg(member.view, "click", 1)
            api.mv.wait_for_movement_start()
            local finishline = find_line(curpos, me:getDest())
            api.mv.move_to(curpos)
            api.mv.wait_for_movement()

            -- Solve for the intersection of both vectors to get distance
            local guess = intersection(startline, finishline)
            local name = string.format("Member %d", member.gobid)
            api.chat.chat_send_message(api.chat.bot_chat(),
                name .. " possible location: " .. guess:toString() .. " -> Distance: " .. (me.rc:dist(guess) / 11))
            api.pointer.make(name, guess)
            api.minimap.mark(name, member.col, guess)

            api.mv.move_to(startpos)
            api.mv.wait_for_movement()
        end
    end
end
