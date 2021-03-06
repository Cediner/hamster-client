-- This API is mostly optional. With how the lua binding works you can access methods/fields directly
-- if you understand the client class layout. This API just simplifies that for people who don't.

local api = {}

--------------------------------------------------
-- Basic constants
--------------------------------------------------
api.const = {
  tilesz = 11,

  leftbutton   = 1,
  mid_button   = 2,
  right_button = 3,

  mf_none           = 0,
  mf_shift          = 1,
  mf_ctrl           = 2,
  mf_shift_ctrl     = 3,
  mf_alt            = 4,
  mf_shift_alt      = 5,
  mf_ctrl_alt       = 6,
  mf_shift_ctrl_alt = 7,

  speed = {
    -- These speeds are based off how many tiles
    -- you *should* go in a single server tick at
    -- the given speed in units.
    -- tiles are made up of units and can be thought of
    -- tilesz x tilesz square units

    -- If you want  to get units/s just do:
    -- (speed / tick_rate) * 1000
    tick_rate   = 60,

    crawl       = 1,
    walk        = 2,
    run         = 3,
    sprint      = 4,

    dugout      = 2,
    boat        = 3,
    snekkja_min = 5,
    snekkja_max = 8,
    knarr_min   = 4,
    knarr_max   = 8
  }
}

--------------------------------------------------
-- Some core helping functions and shortcuts
--------------------------------------------------
api.core = {
  apply = function(arr, fun)
    for i = 1, #arr do
      fun(arr[i])
    end
  end,


  waituntil = function(fun, timeout, refresh)
    refresh = refresh or 100
    local start = script:gettime()
    while not fun() do
      script:sleep(refresh)
      if timeout and script:gettime() - start > timeout then
        return false
      end
    end
    return true
  end,

  gui = function()
    return session:getUI().gui
  end,

  mv = function()
    return session:getUI().gui.map
  end,

  oc = function()
    return session:getUI().sess.glob.oc
  end,

  mc = function()
    return session:getUI().sess.glob.map
  end,

  glob = function()
    return session:getUI().sess.glob
  end
}



--------------------------------------------------
-- Some basic session details about you
--------------------------------------------------
api.session =  {
  username = function()
    return session:username()
  end,

  chrname = function()
    return session:chrname()
  end,

  -- Script Message Queue
  -- Note: filter is a regex filtering on `msg`
  startListening = function(filter, allow_external)
    filter = filter or ".+"
    allow_external = allow_external or false
    script:listen(filter, allow_external)
  end,

  stopListening = function()
    script:stopListening()
    script:clearmsgs()
  end,

  clearMessages = function()
    script:clearmsgs()
  end,

  hasMessage = function()
    return script:hasmsg()
  end,


  -- Messages returned are a class with fields:
  -- `sender` - Widget who sent it to us
  -- `msg`    - The message subject
  -- `args`   - List of arguments based off `msg`
  --
  -- Known Messages
  --- Chat Related
  ---- Bot Chat
  ----- `msg`  : msg
  ----- `args` : (text)
  ---- Area Chat
  ----- `msg`  : area-msg
  ----- `args` : (text, from-name)
  ---- Realm Chat
  ----- `msg`  : realm-msg
  ----- `args` : (text, from-name)
  ---- Village Chat
  ----- `msg`  : village-msg
  ----- `args` : (text, from-name)
  ---- Party Chat
  ----- `msg`  : party-msg
  ----- `args` : (text, from-name)
  ---- Private chat
  ----- Inbound
  ------ `msg` : priv-in-msg
  ------ `args`: (text, from-name)
  ----- Outbound
  ------ `msg` : priv-out-msg
  ------ `args`: (text)
  --- Tile Selection
  ---- `msg`   : bot-select
  ---- `args`  : (starting_coord, ending_coord)
  --- Marking Gobs
  ---- `msg`   : click-gob
  ---- `args`  : (gob_obj)
  --- Marking Tiles
  ---- `msg`   : click-tile
  ---- `args`  : (tile_coord)
  pollMessage = function()
    return script:pollmsg()
  end
}


--------------------------------------------------
-- ObjectCache functionality
--------------------------------------------------
api.oc = {
  getgob = function(id)
    return api.core.oc():getgob(id)
  end,

  getallgobs = function()
    return api.core.oc():getallgobs()
  end,

  posres = function(mc)
    return mc:floor(api.core.oc().posres)
  end
}

--------------------------------------------------
-- MCache functionality
--------------------------------------------------
api.mc = {
  get_tile_z = function(c)
    local mc = api.core.mc()
    return mc:getz_safe(c.div(mc.tilesz))
  end,

  get_tile = function(c)
    local mc = api.core.mc()
    return mc:gettile_safe(c.div(mc.tilesz))
  end,

  get_grid_id = function(c)
    return api.core.mc():getgridid(c)
  end,

  get_tile_offset = function(c)
    return api.core.mc():gettileoffset(c)
  end,

  tilify = function(c)
    local mc = api.core.mc()
    return c:div(mc.tilesz2):mul(mc.tilesz2):add(mc.tilesz2:div(2.0))
  end
}

--------------------------------------------------
-- Character List Widget
--------------------------------------------------
api.charlst = {
  charlst = function()
    return session:getCharlist()
  end,

  login = function(name)
    api.charlst.charlst():wdgmsg('play', name)
  end,

  logout = function()
    api.core.gui():wdgmsg('act', 'lo', 'cs')
  end,
}


--------------------------------------------------
-- Coord helpers
--------------------------------------------------
api.coord = {
  -- x, y are integers
  coord2i = function(x, y)
    return luajava.newInstance("haven.Coord", x, y)
  end,

  -- x, y are doubles
  coord2d = function(x, y)
    return luajava.newInstance("haven.Coord2d", x, y)
  end,

  -- x, y, z are floats
  coord3f = function(x, y, z)
    return luajava.newInstance("haven.Coord3f", x, y, z)
  end

  --Unlike the Lisp API lua makes it easier to just use
  -- class methods, below is the files for Coord, Coord2d,
  -- and Coord3f. You can use any of their `public` functions
  -- Example:
  -- cc = api.coord.coord2i(5, 5)
  -- cc:add(5, 5) -- Coord(10, 10)
  -- cc:mul(5.0)  -- Coord2d(25.0, 25.0)
  --
  -- Coord   : https://gitlab.com/Boshaw/sloth-client/-/blob/sloth/src/haven/Coord.java
  -- Coord2d : https://gitlab.com/Boshaw/sloth-client/-/blob/sloth/src/haven/Coord2d.java
  -- Coord3f : https://gitlab.com/Boshaw/sloth-client/-/blob/sloth/src/haven/Coord3f.java
}

--------------------------------------------------
-- BoundingBox / MapMod functionality
--------------------------------------------------
api.bbox = {
  trigger = function()
    local gui = api.core.gui()
    local pos = api.coord.coord2i(50, 50)
    gui:add(luajava.newInstance("haven.MapMod", pos))
  end,

  make = function(ul, ec)
    local bb = {
      ul = ul,
      ec = ec,
      sz = ec:sub(ul),
      dir = api.coord.coord2i(1, 1)
    }

    if bb.sz.x < 0 then
      bb.dir.x = -1
    end
    if bb.sz.y < 0 then
      bb.dir.y = -1
    end

    return bb
  end,

  ec = function(bb)
    bb.ul:add(api.coord.coord2i( bb.sz.x * session.east, bb.sz.y * session.south ))
  end,

  within = function(bb, c)
    local tsz = api.core.mc().tilesz2
    local sc = bb.ul:mul(tsz)
    local ec = bb.ec:mul(tsz)
    local br = tsz.mul(session.east, session.south)
      :add(api.coord.coord2i( math.max(sc.x, ec.x), math.max(sc.y, ec.y) ))
    local ul = api.coord.coord2i( math.min(sc.x, ec.x), math.min(sc.y, ec.y) )

    return (((c.x >= ul.x) and (c.x <= br.x)) or ((c.x >= br.x) and (c.x <= ul.x)))
      and (((c.y >= ul.y) and (c.y <= br.y)) or ((c.y >= br.y) and (c.y <= ul.y)))
  end,

  gobs = function(bb)
    local gobs = api.oc.getallgobs()
    local ret = {}

    for i = 1, #gobs do
      if api.bbox.within(bb, gobs[i].rc) then
        ret.insert(gobs[i])
      end
    end

    return ret
  end,

  tiles = function(bb)
    local tiles = {}
    local ul = bb.ul:mul(api.core.mc().tilesz2)
    local off = bb.dir:mul(session.east, session.south):mul(api.core.mc().tilesz2)

    for x = 0, math.abs(bb.sz.x) do
      for y = 0, math.abs(bb.sz.y) do
        table.insert(tiles, api.mc.tilify(ul:add(off:mul(x, y))))
      end
    end

    return tiles
  end,

  dots = function(bb)
    local dots = {}
    local ul = bb.ul:mul(api.core.mc().tilesz2)
    local off = bb.dir:mul(session.east, session.south):mul(api.core.mc().tilesz2)

    local function contains(a, e)
      for i = 1, #a do
        if a[i] == e then
          return true
        end
      end
      return false
    end

    local function pushnew(a, e)
      if not contains(a, e) then
        a.insert(e)
      end
    end


    for x = 0, math.abs(bb.sz.x)+1 do
      for y = 0, math.abs(bb.sz.y)+1 do
        local base = api.mc.tilify(ul:add(off:mul(x, y)))

        pushnew(dots, base.add(6,6))
        pushnew(dots, base.add(-5, 6))
        pushnew(dots, base.add(6, -5))
        pushnew(dots, base.add(-5, -5))
      end
    end

    return dots
  end
}


--------------------------------------------------
-- Discord functionality
--------------------------------------------------
api.discord = {
  start_discord_session = function(token)
    session:startDiscord(token)
  end,

  end_discord_session = function()
    session:endDiscord()
  end,

  send_discord_message = function(channel, msg)
    session:sendDiscordMessage(channel, msg)
  end,

  prompt_for_discord_info = function()
    api.session.startListening("discord")
    local token
    local role

    api.core.gui().add(luajava.newInstance("hamster.ui.script.DiscordHelper"),
                       api.coord.coord2i(50, 50))

    repeat
      script:sleep(1000)
      while api.session.hasMessage() do
        local msg = api.session:pollMessage()
        token = msg.args[1]
        role  = msg.args[2]
      end
    until (token and role)

    api.session.stopListening()
    return { token, role }
  end
}


--------------------------------------------------
-- Logging and Chat functionality
--------------------------------------------------
api.chat = {
  const = {
    area = "Area Chat",
    village = "Village",
    party = "Party",
    bot = "Bot-Chat"
  },

  chat = function()
    return api.core.gui().chat
  end,

  area_chat = function()
    return api.chat.chat().area
  end,

  party_chat = function()
    return api.chat.chat().party
  end,

  village_chat = function()
    return api.chat.chat().village
  end,

  bot_chat = function()
    return api.core.gui().botlog
  end,

  realm_chat = function()
    return api.chat.chat().realm
  end,

  privchats = function()
    return api.chat.chat():privchats()
  end,

  privchat_by_name = function(name)
    local chats = api.chat.privchats()
    for i = 1, #chats do
      if name == chats[i]:name() then
        return chats[i]
      end
    end
    return nil
  end,

  sysprint = function(msg)
    api.core.gui():msg(msg)
  end,

  log = function(msg)
    script:log(msg)
  end,

  chat_send_message = function(chat, msg)
    if chat == api.chat.bot_chat() then
      local color = luajava.bindClass("java.awt.Color")
      chat:uimsg("msg", {msg.format("[Bot] %s", msg), color.RED, 1})
    else
      chat:send(msg)
    end
  end
}

--------------------------------------------------
--  Some basic widget functionality
--------------------------------------------------
api.widget = {
  wdgmsg = function(wdg, msg, ...)
    wdg:wdgmsg(msg, {...})
  end,

  uimsg = function(wdg, msg, ...)
    wdg:uimsg(msg, {...})
  end,

  id = function(wdg)
    return wdg:wdgid()
  end,

  add = function(wdg, child, pos)
    pos = pos or api.coord.coord2i(50, 50)
    wdg:add(child, pos)
  end,

  next_wdg_id = function()
    return session:getUI().next_predicted_id
  end,

  ui_force_wdgmsg = function(id, msg, args)
    args = args or {}
    session:getUI():wdgmsg(id, msg, args)
  end
}

--------------------------------------------------
-- Hotkey functionality
--------------------------------------------------
api.hotkey = {
  const = {
    hk_1 = 141,
    hk_2 = 142,
    hk_3 = 143
  },

  use_item = function(slot, mflags)
    api.core.gui():wdgmsg("belt", slot, 1, mflags)
  end,

  set_item = function(slot)
    api.core.gui():wdgmsg("setbelt", slot, 0)
  end,

  unset_item = function(slot)
    api.core.gui():wdgmsg("setbelt", slot, 1)
  end,

  is_hotkey_set = function(slot)
    return api.core.gui().belt[slot]
  end,

  wait_until_hotkey_is_set = function(slot)
    api.core.waituntil((function() return api.hotkey.is_hotkey_set(slot) end))
  end,

  wait_until_hotkey_is_unset = function(slot)
    api.core.waituntil((function() return not api.hotkey.is_hotkey_set(slot) end))
  end
}

--------------------------------------------------
--- Party
--- Party Members have a few funcs/fields you can directly
--- Access:
---  * getgob() [Gob] (Member's Gob obj if within draw range)
---  * getc()  [Coord2d] (Member  position, no z)
---  * geta()  [double]  (Members rotation about itself)
---  * col     [Color] (Color in party)
--- Ex: member:getgob() to get its Gob
--------------------------------------------------
api.party = {
  leader = function()
    return api.core.glob().party:leader()
  end,

  members = function()
    return api.core.glob().party:members()
  end
}

--------------------------------------------------
--- Pointer
--- This is mainly for creating map pointers
--- to aid in showing where something in
--------------------------------------------------
api.pointer = {
  make = function(name, c)
    local ptr = luajava.newInstance("hamster.ui.CustomPointer", name, c)
    ptr:canRemove()
    api.core.gui():add(ptr)
  end
}

--------------------------------------------------
--- Minimap
--- This is to allow marking of the minimap
--------------------------------------------------
api.minimap = {
  mark = function(name, color, mc)
    api.core.gui().mapfile:mark(name, color, mc)
  end,

  mark_with_icon = function(icon, name, color, mc)
    api.core.gui().mapfile:mark(icon, name, color, mc)
  end
}

--------------------------------------------------
-- Gob
-- Gob objects have a few fields you can directly
-- access:
--  * id   [long]    (id)
--  * rc   [Coord2d] (Position)
--  * type [Type]    (Type of gob)
--  * a    [double]  (angle)
-- Ex: g.id to get id
--------------------------------------------------
api.gob = {
  tag = {
    plant = luajava.bindClass("hamster.gob.Tag").PLANT,
    multistage_plant = luajava.bindClass("hamster.gob.Tag").MULTISTAGE_PLANT,
    human = luajava.bindClass("hamster.gob.Tag").HUMAN,
    vehicle = luajava.bindClass("hamster.gob.Tag").VEHICLE,
    water_vehicle = luajava.bindClass("hamster.gob.Tag").WATER_VEHICLE,
    land_vehicle = luajava.bindClass("hamster.gob.Tag").LAND_VEHICLE,
    siege_vehicle = luajava.bindClass("hamster.gob.Tag").SIEGE_VEHICLE,
    animal = luajava.bindClass("hamster.gob.Tag").ANIMAL,
    tamed_animal = luajava.bindClass("hamster.gob.Tag").TAMED_ANIMAL,
    can_pick_up = luajava.bindClass("hamster.gob.Tag").CAN_PICK_UP,
    can_open = luajava.bindClass("hamster.gob.Tag").CAN_OPEN,
    can_ride = luajava.bindClass("hamster.gob.Tag").CAN_RIDE,
    can_fight = luajava.bindClass("hamster.gob.Tag").CAN_FIGHT,
    can_aggro = luajava.bindClass("hamster.gob.Tag").CAN_AGGRO,
    tree = luajava.bindClass("hamster.gob.Tag").TREE,
    bush = luajava.bindClass("hamster.gob.Tag").BUSH,
    log = luajava.bindClass("hamster.gob.Tag").LOG,
    stump = luajava.bindClass("hamster.gob.Tag").STUMP,
    rock = luajava.bindClass("hamster.gob.Tag").ROCK,
    stockpile = luajava.bindClass("hamster.gob.Tag").STOCKPILE
  },

  plgobid = function()
    return api.core.mv().rlplgob
  end,

  mygob = function()
    return api.oc.getgob(api.gob.plgobid())
  end,

  is_moving = function(gob)
    return gob:moving() or (gob.id == api.gob.plgobid() and api.mv.has_moves())
  end,

  ----------------------------------------------------
  -- There's a few gob functions you can call directly
  -- Given Gob `g` these are:
  --
  -- g:getc()  returns Coord3f
  --   - Gets position as seen on the client
  -- g:name()  returns String
  --   - Returns the resource name of this gob
  -- g:getv()  returns double
  --   - Returns the velocity that the gob  is going
  -- g:sdt()   returns int
  --   - Returns the sdt number, used mostly to determine
  --     rendering state of gob (ie: crop stage, drying frame status)
  -- g:overlays()   returns Overlay[]
  --   - Returns an array of current overlays
  --   - For a given Overlay `o` you can do:
  --     o:id()  returns long - Overlay id
  --     o:name() returns String - Overlay resource name
  --
  -- g:hasTag(Tag) returns boolean
  --   - Returns true if the gob has the specified tag, false otherwise
  --   - See: api.gob.tag
  --
  -- States:
  -- g:isDead()
  -- g:isFriendly()
  -- g:isDangerous()
  --
  -- For humans:
  -- g:kinname()
  -- g:equipment()
  --
  -- For Crops:
  -- g:multistageplant()
  -- - Crops that can be harvested 1 before the max stage
  -- g:fallowplant()
  -- - Dead winter plant..
  -- g:getMaxStage()
  --
  ----------------------------------------------------

  ------------------
  -- Overlay helpers
  ------------------
  has_overlay = function(gob, name)
    local ols = gob:overlays()
    for i = 1, #ols do
      if ols[i]:name() == name then
        return true
      end
    end
    return false
  end,

  get_overlay_by_name = function(gob, name)
    local ols = gob:overlays()
    for i = 1, #ols do
      if ols[i]:name() == name then
        return ols[i]
      end
    end
    return nil
  end,

  -----------------------------
  -- The many ways to get gobs
  -----------------------------
  get_by_name = function(name)
    local gobs = api.oc.getallgobs()
    for i = 1, #gobs  do
      if string.find(gobs[i]:name(), name) then
        return gobs[i]
      end
    end
    return nil
  end,

  get_all_by_filter = function (filter)
    local gobs = api.oc.getallgobs()
    local ret = {}
    for i = 1, #gobs do
      if filter(gobs[i]) then
        table.insert(ret, gobs[i])
      end
    end
    return ret
  end,

  get_all_by_name = function(name)
    return api.core.gob.get_all_by_filter((function(g) string.find(g:name(), name) end))
  end,

  get_closest_by_filter_and_path = function(filter)
    local me = api.gob.mygob()
    if me then
      local gobs = api.gob.get_all_by_filter(filter)
      local best
      local bestdist = 999999999
      local mc = me.rc
      for i = 1, #gobs do
        local path = api.mv.find_path_to_gob(gobs[i])
        if path then
          local dist = api.mv.path_distance(path)
          if best == nil or dist < bestdist then
            best = gobs[i]
            bestdist = dist
          end
        end
      end
      return best
    else
      return nil
    end
  end,

  get_closest_by_filter = function(filter)
    local me = api.gob.mygob()
    if me then
      local gobs = api.gob.get_all_by_filter(filter)
      local best
      local mc = me.rc
      if #gobs > 0 then
        best = gobs[1]
        local bestdist = gobs[1].rc:dist(mc)
        for i = 2, #gobs do
          if gobs[i].rc:dist(mc) < bestdist then
            best = gobs[i]
            bestdist = gobs[i].rc:dist(mc)
          end
        end
      end
      return best
    else
      return nil
    end
  end,

  get_closest_by_name_and_path = function(name)
    return api.gob.get_closest_by_filter_and_path((function (g) return string.find(g:name(), name) end))
  end,

  get_closest_by_name = function(name)
    return api.gob.get_closest_by_filter((function (g) return string.find(g:name(), name) end))
  end
}

--------------------------------------------------
-- MapView (movement, pathfinding)
--------------------------------------------------
api.mv = {
  wait_for_movement_start = function(gob)
    gob = gob or api.gob.mygob()
    api.core.waituntil((function() return api.gob.is_moving(gob) end), 2000)
  end,

  wait_for_movement = function(gob)
    gob = gob or api.gob.mygob()
    api.core.waituntil((function() return api.gob.is_moving(gob) end), 2000)
    api.core.waituntil((function() return not api.gob.is_moving(gob) end))
  end,

  -------------------------
  --  Queued movement
  -------------------------
  clear_moves = function()
    api.core.mv():clearmovequeue()
  end,

  queue_move = function(c)
    api.core.mv():queuemove(c)
  end,

  has_moves = function()
    return api.core.mv():hasmoves()
  end,

  -------------------------
  -- Instant Movement
  -------------------------
  move_to = function(c)
    api.core.mv():moveto(c)
  end,

  move_to_rel = function(c)
    api.core.mv():relMove(c)
  end,

  -------------------------
  -- Pathfinding
  -------------------------
  los = function(c)
    return api.core.mv():los(c)
  end,

  los_gob = function(gob)
    return api.core.mv():los(gob)
  end,

  find_path = function(c)
    return api.core.mv():findpath(c)
  end,

  find_path_to_gob = function(gob)
    return api.core.mv():findpath(gob)
  end,

  path_distance = function(moves)
    local len = 0

    if #moves > 0 then
      len = len + moves[1]:dest():dist(api.gob.mygob().rc)
    end

    for i = 2, #moves do
      len = len + moves[i-1]:dest():dist(moves[i]:dest())
    end
    return len
  end,

  walk_path = function(moves)
    local mv = api.core.mv()
    for i = 1, #moves do
      moves[i]:apply(mv)
      api.mv.wait_for_movement()
    end
  end,

  reverse_path = function(move, goalc)
    local i = #move
    local nmoves = {}

    while i > 0 do
      nmoves.insert(move[i])
      i = i - 1
    end

    nmoves.insert(goalc)
    return nmoves
  end,

  path_to = function(c)
    api.mv.walk_path(api.mv.find_path(c))
  end,

  path_to_gob = function(gob)
    api.mv.walk_path(api.mv.find_path_to_gob(gob))
  end,

  smart_move = function(c)
    if api.mv.los(c) then
      api.mv.move_to(c)
      api.mv.wait_for_movement()
    else
      api.mv.path_to(c)
    end
  end,

  smart_move_to_gob = function(gob)
    if api.mv.los_gob(gob) then
      api.mv.move_to(gob.rc)
      api.mv.wait_for_movement()
    else
      api.mv.path_to_gob(gob)
    end
  end,


  -------------------------
  -- Interacting with gobs
  -------------------------
  click_gob = function(gob, btn, mflags, overlayid, fastmeshid)
    overlayid = overlayid or 0
    fastmeshid = fastmeshid or -1

    api.widget.wdgmsg(api.core.mv(), "click", api.coord.coord2i(1, 1), api.oc.posres(gob.rc), btn, mflags,
                         overlayid, gob.id, api.oc.posres(gob.rc), overlayid, fastmeshid)
  end,

  interact_held_item_with_gob = function(gob, mflags, overlayid, fastmeshid)
    overlayid = overlayid or 0
    fastmeshid = fastmeshid or -1

    api.core.mv():wdgmsg("itemact", api.coord.coord2i(1, 1), api.oc.posres(gob.rc), mflags,
                         overlayid, gob.id, api.oc.posres(gob.rc), overlayid, fastmeshid)
  end,

  interact_held_item_with_tile = function(tile, mflags)
    api.widget.wdgmsg(api.core.mv(), "itemact", api.coord.coord2i(1, 1), api.oc.posres(tile), mflags)
  end,

  select_area = function(sc, ec)
    api.core.mv():wdgmsg("sel", sc, ec, api.const.mf_none)
  end,

  drop = function(c, mflags)
    api.core.mv():wdgmsg("drop", api.coord.coord2i(1, 1), api.oc.posres(c), mflags)
  end,

  placing_gob = function()
    return api.core.mv():placing()
  end,

  place_gob = function(c, angle, btn, mflags)
    api.core.mv():wdgmsg("place", api.oc.posres(c), angle, btn, mflags)
  end,

  wait_for_placing_gob = function()
    api.core.waituntil((function() return api.mv.placing_gob() end))
  end,

  wait_for_placing_gob_to_be_gone = function()
    api.core.waituntil((function() return api.mv.placing_gob() == nil end))
  end
}

--------------------------------------------------
-- MenuGrid functionality
--------------------------------------------------
api.menugrid = {
  menu = function()
    return api.core.gui().menu;
  end,

  use = function(paginae)
    api.menugrid.menu():use(paginae)
  end
}

--------------------------------------------------
-- Flowermenu functionality
--------------------------------------------------
api.flowermenu = {
  wait_for_flowermenu = function ()
    api.core.waituntil((function () return session:getFlowermenu() end), 3000)
  end,

  wait_for_no_flowermenu = function ()
    api.core.waituntil((function () return not session:getFlowermenu() end), 3000)
  end,

  select = function (index)
    if session:getFlowermenu() then
      script:wdgmsg(session:getFlowermenu(), "cl", {index, api.const.mf_none})
    end
  end
}

--------------------------------------------------
-- Item Functionality
--------------------------------------------------
api.item = {
  const = {
    size = api.coord.coord2i(33, 33)
  },

  held_item = function ()
    return session:getHeldItem()
  end,

  -------------------------------------------------
  -- Items have a few built in functions and vars
  -- examples:
  --
  -- itm:rnm() - name
  -- itm:isFood()
  -- itm:getContents() - Seed/Liquid/Weight based contents
  -- itm:getRawContents() - array of string of content names
  -- itm.quality
  -- itm.num
  -- itm.meter
  --
  -- For seed/Liquid/Weight based contents you
  -- can use these variables for details:
  -- cont.name
  -- cont.type
  -- cont.current  ( current value )
  -- cont.max
  -------------------------------------------------

  is_item_contents = function (itm, name)
    local conts = itm:getContents()
    if conts then
      return name == conts.name
    else
      return nil
    end
  end,

  position = function(itm)
    return itm:witem().c:div(api.item.const.size)
  end,

  size = function(itm)
    return itm:witem().sz:div(api.item.const.size)
  end,

  inventory = function(itm)
    return itm:witem().parent
  end,

  transfer = function(itm)
    script:wdgmsg(itm, "transfer", { api.coord.coord2i(1, 1), 1 })
  end,

  transfer_all_alike = function(itm)
    script:wdgmsg(itm, "transfer", { api.coord.coord2i(1, 1), -1})
  end,

  drop = function(itm)
    script:wdgmsg(itm, "drop", { api.coord.coord2i(1, 1), 1 })
  end,

  drop_all_alike = function(itm)
    script:wdgmsg(itm, "drop", { api.coord.coord2i(1, 1), -1 })
  end,

  take = function(itm)
    script:wdgmsg(itm, "take", { api.coord.coord2i(1, 1) })
  end,

  interact = function(itm, mflags)
    script:wdgmsg(itm, "iact", { api.coord.coord2i(1, 1), mflags })
  end,

  interact_with_held_item = function(itm, mflags)
    script:wdgmsg(itm, "itemact", { mflags })
  end
}


--------------------------------------------------
-- Inventory Functionality
--------------------------------------------------
api.inventory = {
  main_inventory = function ()
    return session:getMainInventory()
  end,
  study_inventory = function ()
    return session:getStudyInventory()
  end,
  belt_inventory = function ()
    return session:getBeltInventory()
  end,
  number_of_inventories = function ()
    return session:numberOfInventories()
  end,
  get_inventory = function(idx)
    return session:getInventory(idx)
  end,
  get_inventories = function()
    return session:inventories()
  end,
  -----------------------------------------
  -- for Inventories you can also
  -- use these functions below as example
  -- inv:name()
  -- inv:items()
  -- inv:totalSlots()
  -- inv:usedSlots()
  -----------------------------------------

  get_by_name = function(name)
    local invs = api.inventory.get_inventories()
    for i=1, #invs do
      if invs[i]:name() == name then
        return invs[i]
      end
    end
    return nil
  end,

  item_at = function(inv, coord)
    local itms = inv:items()
    for i=1, #itms do
      if coord:between(api.item.position(itms[i]),
                       api.item.position(itms[i]):add(api.item.size(itms[i]):sub(api.coord.coord2i(1, 1)))) then
        return itms[i]
      end
    end
    return nil
  end,

  can_drop_at = function(inv, coord)
    return
  end,

  free_slots = function(inv)
    return inv:totalSlots() - inv:usedSlots()
  end,

  full = function(inv)
    return 0 == (inv:totalSlots() - inv:usedSlots())
  end,

  place_item = function(inv, pos)
    script:wdgmsg(inv, "drop", { pos })
  end,

  transfer_items = function(inv_from, inv_to, amount)
    script:wdgmsg(inv_from, "invxf", { inv_to.id, amount })
  end,

  get_item_by_name = function(inv, name)
    local itms = inv:items()
    for i=1, #itms do
      if name == itms[i]:rnm() then
        return itms[i]
      end
    end
    return nil
  end,

  get_items_by_name = function(inv, name)
    local itms = inv:items()
    local ret = {}
    for i=1, #itms do
      if name == itms[i]:rnm() then
        table.insert(ret, itms[i])
      end
    end

    return ret
  end,

  get_items_by_filter = function(inv, filter)
    local itms = inv:items()
    local ret = {}
    for i=1, #itms do
      if filter(itms[i]) then
        table.insert(ret, itms[i])
      end
    end

    return ret
  end,

  invs_get_items_by_filter = function(filter)
    local ret = {}
    local invs = api.inventory.get_inventories()
    for i=1, #invs do
      local itms = invs[i]:items()
      for j=1, #itms do
        if filter(itms[j]) then
          table.insert(ret, itms[j])
        end
      end
    end
    return ret
  end,

  drop_all_items_by_name = function(inv, name)
    local itms = inv:items()
    for i=1, #itms do
      if name  == itms[i]:rnm() then
        api.item.drop(itms[i])
        script:sleep(50)
      end
    end
  end,

  drop_all_items_alike = function(inv, itm)
    if itm:rnm() ~=  "" then
      api.inventory.drop_all_items_by_name(inv, itm:rnm())
    end
  end,

  invs_drop_all_items_by_name = function(name)
    local invs = api.inventory.get_inventories()
    for i=1, #invs do
      api.inventory.drop_all_items_by_name(invs[i], name)
    end
  end,

  invs_drop_all_items_alike = function(itm)
    api.inventory.invs.drop_all_items_by_name(itm:rnm())
  end,
}

--------------------------------------------------
-- Helpers
--------------------------------------------------
api.helper = {
  sleep = function(time)
    local thr = luajava.bindClass("java.lang.Thread")
    thr:sleep(time)
  end,

  loop_through_messages = function(filterfun)
    local ret = nil
    while ret == null and api.session.hasMessage() do
      if api.session.hasMessage() then
        local msg = api.session.pollMessage()
        if filterfun(msg) then
          ret = msg
        end
      end
    end
    return ret
  end,

  wait_for_filtered_message = function(subj, filterfun)
    api.session.startListening(subj)
    local ret = nil
    while ret == nil do
      ret = api.helper.loop_through_messages(filterfun)
      api.helper.sleep(1000)
    end
    api.session.clearMessages()
    api.session.stopListening()
  end,

  wait_for_message = function(subj)
    return api.helper.wait_for_filtered_message(subj, function (msg) return true end)
  end,

  prompt_for_input = function(msg)
    api.chat.chat_send_message(api.chat.bot_chat, msg)
    return api.helper.wait_for_message("(^msg$)").args
  end,

  prompt_for_coord = function(msg)
    api.chat.chat_send_message(api.chat.bot_chat, msg)
    return api.helper.wait_for_message("(^click-tile$)").args
  end,

  wait_progress = function()
    while api.core.gui().prog == -1 do
      api.helper.sleep(100)
    end
    while api.core.gui().prog ~= -1 do
      api.helper.sleep(100)
    end
  end
}

--------------------------------------------------
--------------------------------------------------

return api
