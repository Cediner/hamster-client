local api = require("data.scripts.lualib.api")

function get_area()
    api.session.startListening("bot-select", true)
    api.bbox.trigger()
    while true do
        if api.session.hasMessage() then
            local msg = api.session.pollMessage()
            api.session.stopListening()
            return api.bbox.make(msg.args[1], msg.args[2])
        end
        api.helper.sleep(1000)
    end
end

function is_harvestable(gob)
    return gob:sdt() == gob:getMaxStage() or (gob:multistageplant() and gob:sdt() > 0)
end

function is_seed(itm, qty)
    qty = qty or 0
    if string.find(itm:resource().name, "seed") then
        return itm:getinfo(luajava.bindClass("haven.GItem$Amount")):get():itemnum() > qty
    end
    return (string.find(itm:resource().name, "carrot") or
            string.find(itm:resource().name, "leek") or
            string.find(itm:resource().name, "beet") or
            string.find(itm:resource().name, "turnip") or
            string.find(itm:resource().name, "onion")) ~= nil
end

function get_seed()
    local inv = api.inventory.main_inventory()
    local itms = inv:items()
    local max = 0
    local max_itm
    for i=1, #itms do
        if is_seed(itms[i], 4) and itms[i]:quality() > max then
            max = itms[i]:quality()
            max_itm = itms[i]
        end
    end
    return max_itm
end

function drop_seeds()
    local inv = api.inventory.main_inventory()
    local itms = inv:items()
    for i=1, #itms do
        if is_seed(itms[i]) then
            api.item.drop(itms[i])
        end
    end
end

function replant(tiles_to_replant)
    for i = 1, #tiles_to_replant do
        api.mv.move_to(api.coord.coord2d(tiles_to_replant[i].x, tiles_to_replant[i].y))
        api.mv.wait_for_movement()

        local item = get_seed()
        if item ~= nil then
            local pos = api.item.position(item)
            api.item.take(item)
            while api.item.held_item() == nil do
                api.helper.sleep(100)
            end
            api.mv.interact_held_item_with_tile(api.coord.coord2d(tiles_to_replant[i].x, tiles_to_replant[i].y), 0)
            -- wait for gob under
            local dist = 9999
            while dist > 6 do
                local gob = api.gob.get_closest_by_name("gfx/terobjs/plants")
                dist = gob.rc:dist(api.gob.mygob().rc)
                api.helper.sleep(200)
            end
            if api.item.held_item ~= nil then
                api.inventory.place_item(api.inventory.main_inventory(), pos)
                while api.item.held_item() ~= nil do
                    api.helper.sleep(100)
                end
            end
            api.helper.sleep(100)
        end
    end

    drop_seeds()
    for k,v in pairs(tiles_to_replant) do tiles_to_replant[k]=nil end
end

local tiles = api.bbox.tiles(get_area())
local replant_tiles = {}
for i = 1, #tiles do
    api.mv.move_to(api.coord.coord2d(tiles[i].x, tiles[i].y))
    api.mv.wait_for_movement()
    table.insert(replant_tiles, tiles[i])

    local gob = api.gob.get_closest_by_name("gfx/terobjs/plants")
    local dist = gob.rc:dist(api.gob.mygob().rc)
    if dist < 6 and is_harvestable(gob) then
        api.mv.click_gob(gob, 3, 0)
        api.helper.wait_progress()
    end

    if api.inventory.free_slots(api.inventory.main_inventory()) < 3 then
        replant(replant_tiles)
    end
end
replant(replant_tiles)

