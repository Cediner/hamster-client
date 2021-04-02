package integrations.mapv4;

import haven.Utils;
import integrations.json.JSONArray;
import integrations.json.JSONObject;

public class MapConfig {
    public static boolean loadMapSetting(String username, String type) {
	try {
	    String loginsjson = Utils.getpref("json-vendan-mapv4", null);
	    if (loginsjson == null) {
		JSONArray ja = new JSONArray();
		JSONObject jo = new JSONObject();
		jo.put("name", username);
		jo.put(type, true);
		ja.put(jo);
		Utils.setpref("json-vendan-mapv4", ja.toString());
		return (true);
	    }
	    JSONArray ja = new JSONArray(loginsjson);
	    for (int i = 0; i < ja.length(); i++) {
		JSONObject jo = ja.getJSONObject(i);
		if (jo.getString("name").equals(username)) {
		    boolean bol = true;
		    try {
			bol = jo.getBoolean(type);
		    } catch (Exception ignored) {
		    }
		    return (bol);
		}
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return (true);
    }

    public static void saveMapSetting(String username, boolean bol, String type) {
	try {
	    String loginsjson = Utils.getpref("json-vendan-mapv4", null);
	    JSONArray ja;
	    if (loginsjson == null) {
		ja = new JSONArray();
		JSONObject jo = new JSONObject();
		jo.put("name", username);
		jo.put(type, bol);
		ja.put(jo);
	    } else {
		boolean isExist = false;
		ja = new JSONArray(loginsjson);
		for (int i = 0; i < ja.length(); i++) {
		    JSONObject jo = ja.getJSONObject(i);
		    if (jo.getString("name").equals(username)) {
			jo.put(type, bol);
			isExist = true;
			break;
		    }
		}
		if (!isExist) {
		    JSONObject jo = new JSONObject();
		    jo.put("name", username);
		    jo.put(type, bol);
		    ja.put(jo);
		}
	    }
	    Utils.setpref("json-vendan-mapv4", ja.toString());
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
