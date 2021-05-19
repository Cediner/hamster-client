local api = require("data.scripts.lualib.api")

function get_dead_bats()
    return api.inventory.invs_get_items_by_filter((function(itm) return itm:rnm() == "Dead Bat" end))
end

function get_clean_bats()
    return api.inventory.invs_get_items_by_filter((function(itm) return itm:rnm() == "Cleaned Bat" end))
end

function do_act(itms)
    local id = api.widget.next_wdg_id()
    for i=1, #itms do
        api.item.interact(itms[i], api.const.mf_none)
        api.widget.ui_force_wdgmsg(id, "cl", { 0, api.const.mf_none })
        id = id + 1 + 2
    end
end

-- Clean
local itms = get_dead_bats()
while #itms > 0 do
    do_act(itms);
    script:sleep(500)
    itms = get_dead_bats()
end

-- Butcher
itms = get_clean_bats()
while #itms > 0 do
    do_act(itms);
    script:sleep(500)
    itms = get_clean_bats()
end