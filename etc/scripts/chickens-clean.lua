local api = require("data.scripts.lualib.api")

function get_itms()
  return api.inventory.invs_get_items_by_filter((function(itm) return itm:rnm() == "Plucked Chicken" end))
end


itms = get_itms()
while #itms > 0 do
  local id = api.widget.next_wdg_id()
  for i=1, #itms do
    api.item.interact(itms[i], api.const.mf_none)
    api.widget.ui_force_wdgmsg(id, "cl", { 0, api.const.mf_none })
    id = id + 1 + 2
  end
  script:sleep(500)
  itms = get_itms()
end
