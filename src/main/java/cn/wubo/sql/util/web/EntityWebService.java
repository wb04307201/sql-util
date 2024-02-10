package cn.wubo.sql.util.web;

import cn.wubo.sql.util.SQL;
import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.exception.EntityWebException;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class EntityWebService {

    private static Map<String, Class<?>> classMap = new ConcurrentHashMap<>();

    public TableModel view(String id) {
        if (classMap.containsKey(id)) return EntityUtils.getTable(classMap.get(id));
        else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

    public void build(String id, Class<?> clazz) {
        classMap.put(id, clazz);
    }

    public Object select(String id, EntityWebQuery query) {
        if (classMap.containsKey(id)) {
            TableModel tableModel = EntityUtils.getTable(classMap.get(id));
            SQL<?> sql = new SQL<>(classMap.get(id)).select();


            return null;
        } else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

}
